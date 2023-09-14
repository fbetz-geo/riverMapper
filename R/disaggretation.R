#' Disaggregate the river corridor
#' @description uses centerline of the river corridor and Thiessen polygons to create disaggregated river segments
#' @param centerline sf line object representing the centerline of the river corridor; it is assumed to be in projected coordinates with metric units
#' @param segment_length spacing of the segments for the disaggregated river corridor, defaults to 100 m
#' @param max_width estimation of the maximum width of the river corridor to ensure that the polygons cover the entire corridor
#' @author Florian Betz
#' @references Alber, A., Piégay, H. (2011): Spatial disaggregation and aggregation procedures for characterizing
#'              fluvial features at the network scale: applications to the Rhône basin (France). Geomorphology 125, 343-360.
#'              Betz, F., Lauermann, M., Cyffka, B. (2020): Open source riverscapes:...
#' @return returns a sf polygon object  
#' 

disaggregate<-function(centerline,segment_length=100,max_width)
{
  pts<-sf::st_line_sample(centerline,density = 1/segment_length,type = "regular")
  v_raw<-st_voronoi(pts) %>% st_cast()
  v<-st_intersection(v_raw,st_buffer(centerline,max_width))
  v$ID<-1:length(v)
  return(v)
}