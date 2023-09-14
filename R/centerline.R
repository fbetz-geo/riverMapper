#' creates a centerline from the raster based corridor mask
#' @description The centerline function derives the centerline of the river corridor mask by 
#' @param mask spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA
#' @param smooth smoothing factor for converting the raster mask to polygon 
#' @author Florian Betz
#' @return a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
#' 

centerline<-function(mask,smooth){
  
  #vectorizing the raster mask
  poly<-terra::as.polygons(mask) %>% sf::st_as_sf
  
  #Simplify the geometry
  poly_simple<-sf::st_simplify(poly,dTolerance = smooth)
}