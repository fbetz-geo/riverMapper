#' creates segments from the centerline for working further with the data
#' @description The disaggregation function creates the disaggregated segments of the river corridor in a specified segment length; 
#' from a conceptual perspective, these segments represent a longitudinal continuum along the river
#' @param C_line sf object representing the centerline as derived from the centerline() function
#' @param mask spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA; 
#' alternatively, mask can also be a sf object with a polygon representing the river corridor 
#' @param seg_length length of the desired river segments; choosing the appropriate scale of analysis will depend very much 
#' on the resolution of the input data and the scope of the specific analysis; default is 1 km
#' @author Florian Betz
#' @return a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
#' @export segmentation
#' 

disaggregation<-function(c_line,mask,seg_length=1000){
  
  #Create polygon from the mask
  if (is(mask,"SpatRaster")) {
    poly<-terra::as.polygons(mask)
  }else{
    poly<-vect(mask)
  }
  
  #Create points in the specified distance from the centerline and convert to terra::vect() format to avoid errors with sf::st_voronoi
  pts<-sf::st_segmentize(c_line,dfMaxLength = seg_length) %>% sf::st_coordinates() %>% as.data.frame() %>% 
    sf::st_as_sf(coords=c("X","Y"),crs=crs(riverline)) %>% terra::vect()
  
  #Create Voronoi polygons from the points
  segs<-terra::voronoi(pts,bnd=poly) %>% sf::st_as_sf() %>% dplyr::mutate(Seg_ID=dplyr::row_number())
  
  return(segs)
} 