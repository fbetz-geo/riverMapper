#' Creates a longitudinal profile using a DEM (or other raster) and a channel line as input
#' @description This function creates longitudinal profiles from a raster (most likely a DEM) and a channel line.
#' It uses the profile_dist() function to compute distances along the line, extracts the raster values
#' and adds them to a data.frame with the coordinates and cumulative distances along the line as well as the raster values
#' @param channel_line sf object containing the channel line 
#' @param rast_file terra object with the raster values to use for the profile
#' @param plot If TRUE, a line plot of the profile will be returned
#' @author Florian Betz
#' @return a data.frame with the coordinates, the profile distance and the raster value
#' @export make_profile
#' 

make_profile<-function(channel_line, rast_file,plot=TRUE){
  
  #Compute the distances along channel
  dists<-profile_dist(channel_line)
  
    #Check for multilayer, which is not supported yet
  if (terra::nlyr(rast_file)>1) {
    stop("Currently only single layer rasters are supported. Please select the appropriate layer to create the profile")
  }
  
  #rename layer in the rast-file to set proper name for the column
  names(rast_file)<-"value"
  
  #extract the raster values
  vals<-terra::extract(rast_file, terra::vect(dists))
  
  #Assign values as column to the original file
  dists$value<-vals$value
  
  return(dists)
  
  if (plot) {
    plot(dists$dists,dists$value,type="l",x="Profile Distance", y="Value")
    
  }
  
}