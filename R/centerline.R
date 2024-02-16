#' creates a centerline from the raster based corridor mask
#' @description The centerline function derives the centerline of the river corridor mask by 
#' @param mask spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA
#' @param smooth smoothing factor for converting the raster mask to polygon, a higher smooth parameter will result in higher degree of simplification of the polygon
#' @param distance Interval for inserting new vertices to the outline; see also densify_sf; default is 100, assuming that the data is in projected coordinate system with the units being meters.
#' @author Florian Betz
#' @return a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
#' 

centerline<-function(mask,smooth, distance=100){
  
  #vectorizing the raster mask
  poly<-terra::as.polygons(mask) %>% sf::st_as_sf
  
  #Simplify the geometry
  poly_simple<-sf::st_simplify(poly,dTolerance = smooth)
  
  #Convert the polygon to line
  outline<-sf::st_boundary(poly_simple)
  
  #Densify the outline
  outline_dense<-riverMapper::densify_sf(outline, distance=distance)
  
}