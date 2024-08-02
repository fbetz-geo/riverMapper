#' creates a centerline from the raster based corridor mask
#' @description The centerline function derives the centerline of the river corridor mask by 
#' @param mask spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA
#' @param riverline sf object representing the river line
#' @param smooth smoothing factor for converting the raster mask to polygon, a higher smooth parameter will result in higher degree of simplification of the polygon
#' @param distance Interval for inserting new vertices to the outline; see also densify_sf; default is 100, assuming that the data is in projected coordinate system with the units being meters.
#' @author Florian Betz
#' @return a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
#' @export centerline
#' 

centerline<-function(mask,riverline, smooth=500, distance=100){
  
  #vectorizing the raster mask
  poly<-terra::as.polygons(mask) %>% sf::st_as_sf()
  
  #Simplify the geometry
  poly_simple<-sf::st_simplify(poly,dTolerance = smooth)
  
  #Convert the polygon to line
  outline<-sf::st_boundary(poly_simple)
  
  #Extend the river line for splitting the outline
  riv_extended<-riverMapper::extend_line(riverline)
  
  
  #Split outline and create base points for voronoi polygons
  split_lines<-lwgeom::st_split(outline,riv_extended)
  
  v_points<-sf::st_segmentize(split_lines,dfMaxLength = distance) %>% 
    st_cast("POINTS")
  
  #Create Voronoi polygons from the points
  voros<-sf::st_voronoi(v_points) %>% sf::st_cast("LINESTRING")
  
  #split Voronoi polygons into two parts
  voro_1<-voros %>% dplyr::filter()
  
  #Merge Thiessen Polygons and extract centerline
  centerline<-sf::st_intersect()
  
  return()
  
}
