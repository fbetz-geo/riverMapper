#' creates a centerline from the raster based corridor mask
#' @description The centerline function derives the centerline of the river corridor mask
#' @param mask spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA; 
#' alternatively, mask can also be a sf object with a polygon representing the river corridor 
#' @param riverline sf object representing the river line
#' @param seg_length length of the desired river segments; choosing the appropriate scale of analysis will depend very much 
#' on the resolution of the input data and the scope of the specific analysis; default is 1 km
#' @param smooth_factor smoothing factor for converting the raster mask to polygon, a higher smooth parameter will result in higher degree of simplification of the polygon
#' @param d length to extend the ends of the riverline in order to split the corridor outline; can be adjusted to avoid errors
#' @return a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
#' @export centerline
#' 

centerline<-function(mask,riverline, seg_length=1000, smooth_factor=2500, d=5000){
  
  
  #Remove all attributes from the riverline to avoid errors
  riverline<-sf::st_geometry(riverline) %>% sf::st_sf()
  
  #vectorizing the raster mask if given as spatRaster
  if (is(mask,"SpatRaster")) {
    poly<-terra::as.polygons(mask) %>% sf::st_as_sf()
  }else{
    poly<-mask
  }
  
  #Convert the polygon to line
  outline<-sf::st_boundary(poly)
  
  #Extend the river line for splitting the outline
  riv_extended<-riverMapper::extend_line(riverline,d=d)
  
  
  #Split outline
  splits<-lwgeom::st_split(outline,riv_extended) %>% sf::st_collection_extract("LINESTRING") %>% 
    
    #Smooth the line in order to obtain a smooth centerline
    sf::st_simplify(dTolerance = smooth_factor) %>% 
    
    #densify the vertices of the line to get enough points for the voronoi polygon creation
    sf::st_segmentize(dfMaxLength = seg_length)
  
  #This will result in a line object with three features and we will need to re-combine the two shorter line features
  l<-sf::st_length(splits) %>% order()
  
  c_points<-sf::st_union(splits[l[1],],splits[l[2],]) %>% 
    
    #convert to points
    st_coordinates() %>% as.data.frame() %>% sf::st_as_sf(coords=c("X","Y"),crs=crs(splits)) %>% 
    
    #Add ID column for distinguishing between the two sides of the river and drop columns arising from splitting
    dplyr::mutate(ID=1) %>% dplyr::select(-c(L1,L2))
  
  r_points<-splits[l[3],] %>% 
    
  #Convert also to points
    st_coordinates() %>% as.data.frame() %>% sf::st_as_sf(coords=c("X","Y"),crs=crs(splits)) %>% 
    
  #And add ID column and drop columns arising from splitting
    dplyr::mutate(ID=2) %>% dplyr::select(-L1)

  #Combine to single point object and convert to terra vect() format to avoid errors with sf::st_voronoi()
  v_points<-rbind(c_points,r_points) %>% terra::vect()
  
  
  #Create Voronoi polygons from the points
  voros<-terra::voronoi(v_points)
  
  #split Voronoi polygons into two parts
  voro_1<-terra::subset(voros,voros$ID==1) %>% terra::as.lines()
  voro_2<-terra::subset(voros,voros$ID==2) %>% terra::as.lines()
  
  #Merge Thiessen Polygons and extract centerline
  centerline<-terra::intersect(voro_1,voro_2) %>% terra::crop(poly) %>% sf::st_as_sf() %>% dplyr::select(-c(ID,ID.1)) %>% st_simplify(dTolerance = smooth_factor)
  
  return(centerline)
  
}
