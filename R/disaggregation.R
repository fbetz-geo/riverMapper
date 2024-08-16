#' creates segments from the centerline for working further with the data
#' @description The disaggregation function creates the disaggregated segments of the river corridor in a specified segment length; 
#' from a conceptual perspective, these segments represent a longitudinal continuum along the river
#' @param c_line sf object representing the centerline as derived from the centerline() function
#' @param mask spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA; 
#' alternatively, mask can also be a sf object with a polygon representing the river corridor 
#' @param seg_length length of the desired river segments; choosing the appropriate scale of analysis will depend very much 
#' on the resolution of the input data and the scope of the specific analysis; default is 1 km
#' @param start where should the profile distance start? "Highest" will start compute the distances from source to mouth, 
#'"lowest" from mouth to source
#' @param dem path to a digital elevation model stored in any common raster data format, this is required for determining start and end by elevation
#' @author Florian Betz
#' @return a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
#' @export disaggregation
#' 

disaggregation<-function(c_line,mask,seg_length=1000,start="lowest",dem){
  
  #Create polygon from the mask
  if (is(mask,"SpatRaster")) {
    poly<-terra::as.polygons(mask) %>% terra::fillHoles()
  }else{
    poly<-vect(mask) %>% terra::fillHoles()
  }
  
  #Temporary store coordinate system
  c_rs<-sf::st_crs(c_line)
  
  #Check if the centerline is only one feature
  if (nrow(c_line)>1) {
    c_line<-sf::st_union(c_line)
    
  }

  #Convert centerline to sf LINESTRING if necessary
  if(sf::st_geometry_type(c_line)=="MULTILINESTRING"){
    c_line<-sf::st_line_merge(c_line)
  }
    
  #Create points in the specified distance from the centerline and convert to terra::vect() format to avoid errors with sf::st_voronoi
  pts<-sf::st_line_sample(c_line, density = 1/seg_length,type = "regular") %>% terra::vect()
  
  #Create Voronoi polygons from the points
  segs_init<-terra::voronoi(pts,bnd=poly) %>% terra::intersect(poly)  %>% sf::st_as_sf() %>% 
    dplyr::mutate(seg_id=dplyr::row_number())
  
  #Compute distances along centerline
  c_line_dist<-riverMapper::profile_distance(riverline=c_line,maxDist = seg_length/2,start=start,dem=dem)
  
  
  #Make spatial join of distance object and segments
  segs_with_lengths<-sf::st_join(segs_init,c_line_dist) %>% 
    dplyr::group_by(seg_id) %>% 
    dplyr::summarize(centerline_distance=median(distances)) %>% 
    dplyr::arrange(centerline_distance)
  
  segs<-segs_with_lengths %>% dplyr::select(centerline_distance) %>% dplyr::mutate(segment_id=dplyr::row_number())
  
  return(segs)
} 