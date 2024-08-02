#'This is a helper function for constructing the centerline. Its intended for internal use only
#'@description This function extends a line to both ends in the direction of the last line segment by a specified distance
#'The function assumes the line to be in projected coordinates
#'@param line sf object containing the line to be extended
#'@param dist the distance in map units to extend the line
#'@author Florian Betz
#'@return an sf object containing the extended line
#'@export extend_line
#'


extend_line <- function(line, dist) {
  
  #Dissolve the line
  line<-sf::st_union(line) %>% sf::st_cast("LINESTRING")
  
  # Calculate the direction vectors for both ends
  start <- sf::st_point(sf::st_coordinates(line)[1,])
  end <- sf::st_point(sf::st_coordinates(line)[nrow(sf::st_coordinates(line)),])
  
  dir_start <- sf::st_coordinates(line)[2,] - sf::st_coordinates(line)[1,]
  dir_end <- sf::st_coordinates(line)[nrow(sf::st_coordinates(line)),] - sf::st_coordinates(line)[nrow(sf::st_coordinates(line)) - 1,]
  
  # Normalize the direction vectors
  dir_start <- dir_start / sqrt(sum(dir_start^2))
  dir_end <- dir_end / sqrt(sum(dir_end^2))
  
  # Create new points at both ends
  new_start <- start - (dist * dir_start)
  new_end <- end + (dist * dir_end)
  
  # Combine the new points with the original line coordinates
  new_coords <- rbind(new_start, sf::st_coordinates(line), new_end)
  
  # Create a new line with the extended coordinates
  new_line<-new_coords %>% as.data.frame() %>% sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(riverline)) %>% 
    dplyr::summarize(do_union=FALSE) %>% sf::st_cast("LINESTRING")
  return(new_line)
}
