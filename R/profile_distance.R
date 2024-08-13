#' Computing distance along a channel
#' @description
#' This function computes the distance along a channel line, where the line is "exploded" before computing distances. The function
#' assumes the channel line being in projected coordinates and will give an error if this is not the case. 
#'@param riverline sf object containing the channel line
#'@param maxDist maximum distance (in meter) between two consecutive vertices when densifying the vertices of the line. 
#'Default is 10 m.In general, maxDist should be in a similar range as the resolution of the underlying elevation data. 
#'@author Florian Betz
#'@return an sf points object containing the downstream distances along the channel line along with their downstream distance
#'@export profile_distance
#'
profile_distance<-function(riverline, maxDist=10){

  #Check for projected coordinates
  if (sf::st_is_longlat(x)) {
    stop("profile_distance expects a projected coordinate system for computing distances. Please consider reprojecting")
  
  }
    
  #Densify line vertices
  dist_line<-sf::st_segmentize(x,dfMaxLength = units::set_units(maxDist,m))
  
  #Extract coordinates and compute distances
  dists<-sf::st_coordinates(dist_line) %>% as.data.frame() %>% 
    dplyr::mutate(distances=cumsum(c(0,sqrt(diff(X)^2 + diff(Y)^2))))
  
  return(dists)
  
}
