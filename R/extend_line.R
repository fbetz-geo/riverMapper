#'This is a helper function for constructing the centerline. Its intended for internal use only
#'@description This function extends a line to both ends in the direction of the last line segment by a specified dance
#'The function assumes the line to be in projected coordinates
#'@param riverline sf object containing the line to be extended
#'@param d the dance in map units to extend the line
#'@author Florian Betz
#'@return an sf object containing the extended line
#'@export extend_line
#'


extend_line <- function(riverline, d) {
  
  #Check, if coordinate system is project and throw error if it is not
  if (sf::st_is_longlat(riverline)) {
    stop("This function is working only with projected coordinates. Please consider reprojecting")
    
  }
  
  #Store coordinate system for grabbing it later
  cs<-sf::st_crs(riverline)
  
  #Dissolve the riverline
  riverline<-sf::st_union(riverline) %>% sf::st_cast("LINESTRING")
  
  # Calculate the direction vectors for both ends
  start <- sf::st_point(sf::st_coordinates(riverline)[1,])
  end <- sf::st_point(sf::st_coordinates(riverline)[nrow(sf::st_coordinates(riverline)),])
  
  dir_start <- sf::st_coordinates(riverline)[2,] - sf::st_coordinates(riverline)[1,]
  dir_end <- sf::st_coordinates(riverline)[nrow(sf::st_coordinates(riverline)),] - sf::st_coordinates(riverline)[nrow(sf::st_coordinates(riverline)) - 1,]
  
  # Normalize the direction vectors
  dir_start <- dir_start / sqrt(sum(dir_start^2))
  dir_end <- dir_end / sqrt(sum(dir_end^2))
  
  # Create new points at both ends
  new_start <- start - (d * dir_start)
  new_end <- end + (d * dir_end)
  
  # Combine the new points with the original riverline coordinates
  line1 <- rbind(new_start, start) %>% as.data.frame() %>% sf::st_as_sf(coords=c("X","Y"),crs=cs) %>% 
    sf::st_union() %>% sf::st_cast("LINESTRING")
  line2 <- rbind(new_end, end) %>% as.data.frame() %>% sf::st_as_sf(coords=c("X","Y"),crs=cs) %>% 
    sf::st_union() %>% sf::st_cast("LINESTRING")
  
  # Create a new line with the extended coordinates
  new_line<-sf::st_union(line1,line2)
  return(new_line)
}
