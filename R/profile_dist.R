#' Computing distance along a channel
#' @description
#' This function computes the distance along a channel line, where the line is "exploded" before computing distances. The function
#' assumes the channel line being in projected coordinates and will give an error if this is not the case. 
#'@param x sf object containing the channel line
#'@param maxDist maximum distance (in meter) between two consecutive vertices when densifying the vertices of the line. 
#'Default is 10 m.In general, maxDist should be in a similar range as the resolution of the underlying elevation data. 
#'@author Florian Betz
#'
profile_distance<-function(x, maxDist=10){

  #Check for projected coordinates
  if (sf::st_is_longlat()) {
    stop("profile_distance expects a projected coordinate system for computing distances. Please consider reprojecting")
  
  }
    
  #Explode line
  dist_line<-sf::st_segmentize(x,dfMaxLength = units::set_units(maxDist,m))
  
  #Convert to points as basis for distance calculation
  dist_pts<-sf::st_cast(dist_line,"POINT")
  
  #Compute distances
  dists<-sf::st_coordinates(dist_pts) %>% as.data.frame() %>% 
    dplyr::mutate(dists=cumsum(0,sqrt(diff(coords[,"X"])^2 + diff(coords[,"Y"])^2)))
  
  return(dists)
  
}