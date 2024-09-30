#' Creates an orthogonal line on another line
#' @description This function creates an orthogonal line on another line and is designed to work as a 
#' helper function in the channel and corridor width functions
#' @param line sf object of the line used to construct the orthogonal line on
#' @param line_length the length of the orthogonal line
#' @author Florian Betz
#' @return an sf object of the orthogonal line
#'@export ortho_line
#'

ortho_line<-function(line, line_length){
  
  #Temporary store coordinate system
  c_rs<-sf::st_crs(line)
  
  #Dissolve line
  line<-sf::st_union(line)
  
  #Extract the coordinates from the line
  coords<-sf::st_coordinates(line)
  
  #Find the midpoint of the base line
  distances<-c(0, cumsum(sqrt(rowSums(diff(coords)^2))))
  
  mid_length<-max(distances)/2
  
  midpoint_index<-which(distances>=mid_length)[1]
  
  # Calculate the proportion along the segment where the midpoint lies
  proportion <- (mid_length - distances[midpoint_index - 1]) / 
    (distances[midpoint_index] - distances[midpoint_index - 1])
  
  # Calculate the midpoint coordinates
  midpoint <- coords[midpoint_index - 1, ] + 
    proportion * (coords[midpoint_index, ] - coords[midpoint_index - 1, ])
  
  # Calculate the tangent direction at the midpoint
  tangent_direction <- coords[midpoint_index, ] - coords[midpoint_index - 1, ]
  
  # Calculate the orthogonal direction (rotate by 90 degrees)
  orthogonal_direction <- c(-tangent_direction[2], tangent_direction[1])
  
  # Normalize the orthogonal direction
  orthogonal_direction <- orthogonal_direction / sqrt(sum(orthogonal_direction^2))
  
  # Calculate the end points of the orthogonal line (both directions)
  end_point_1 <- midpoint + line_length * orthogonal_direction
  end_point_2 <- midpoint - line_length * orthogonal_direction
  
  #Create the outputline
  orthogonal_line <- st_sfc(st_linestring(rbind(end_point_1, end_point_2)), crs = c_rs) %>% sf::st_as_sf()
  
  return(orthogonal_line)
}
