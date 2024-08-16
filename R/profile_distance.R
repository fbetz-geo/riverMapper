#' Computing distance along a channel
#' @description
#' This function computes the distance along a channel line, where the line is "exploded" before computing distances. The function
#' assumes the channel line being in projected coordinates and will give an error if this is not the case. 
#'@param riverline sf object containing the channel line
#'@param start where should the profile distance start? "Highest" will start compute the distances from source to mouth, 
#'"lowest" from mouth to source
#'@param dem path to a digital elevation model stored in any common raster data format, this is required for determining start and end by elevation
#'@param maxDist maximum distance (in meter) between two consecutive vertices when densifying the vertices of the line. 
#'Default is 10 m.In general, maxDist should be in a similar range as the resolution of the underlying elevation data. 
#'@author Florian Betz
#'@return an sf points object containing the downstream distances along the channel line along with their downstream distance
#'@export profile_distance
#'
profile_distance<-function(riverline, start="highest", dem,maxDist=10){

  #Check for projected coordinates
  if (sf::st_is_longlat(riverline)) {
    stop("profile_distance expects a projected coordinate system for computing distances. Please consider reprojecting")
  
  }
  
  #Temporary store coordinate system including check if coordinate system object exists already
  if (!exists("c_rs")) {
    c_rs<-sf::st_crs(riverline)
  }
  
  #Densify line vertices
  dist_line<-sf::st_segmentize(riverline,dfMaxLength = units::set_units(maxDist,m))
  
  #Extract coordinates and compute distances
  elev_pts<-sf::st_coordinates(dist_line) %>% as.data.frame() %>% 
    
    #Make points to extract values from DEM
    sf::st_as_sf(coords=c("X","Y"),crs=c_rs)
  
  #Grab elevation values from DEM and sort data.frame
  dem<-rast(dem)
  elev_pts$elev<-NA
  elev_pts$elev[1]<-terra::extract(dem, terra::vect(elev_pts[1,]))[1,2]
  elev_pts$elev[nrow(elev_pts)]<-terra::extract(dem, terra::vect(elev_pts[nrow(elev_pts),]))[1,2]
  elev_pts$elev<-stats::approx(x = 1:nrow(elev_pts),y=elev_pts$elev,xout=1:nrow(elev_pts))$y
  
  #For highest to lowest, i.e. source to mouth
  if (start=="highest") {
    elev_pts<-elev_pts %>% dplyr::arrange(dplyr::desc(elev))
  }
  
  #For lowest to highest, i.e. mouth to source
  if (start=="lowest") {
    elev_ots<-elev_pts %>% dplyr::arrange(elev)
    
  }
  dists<-sf::st_coordinates(elev_pts) %>% as.data.frame() %>% 
    dplyr::mutate(distances=cumsum(c(0,sqrt(diff(X)^2 + diff(Y)^2)))) %>% 
    sf::st_as_sf(coords=c("X","Y"),crs=c_rs)
  
  return(dists)
  
}
