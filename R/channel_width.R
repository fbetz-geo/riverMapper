#' Estimates the width of the active channel
#' @description The centerline function derives the centerline of the river corridor mask
#' @param c_line centerline object derived with the centerline() function
#' @param dgo an sf object with the disaggregated river corridor derived with the disaggregation() function
#' @param water_mask SpatRaster or file path to a binary raster representing an approximation of bankfull discharge;
#' in a simple case, this raster can be obtained from thresholding over the JRC global surface water dataset (https://global-surface-water.appspot.com)
#' @param max_width estimated maximum width of the river corridor; in doubt increase, it is clipped to the dgo outline
#' @author Florian Betz
#' @return a data.frame with two columns, the segment id of the dgo object and the active channel width (acw)
#' @export channel_width
#' 

channel_width<-function(c_line, dgo, water_mask, max_width=15000){
  
  #Check if the centerline is in the correct format and convert if necessary
  if (nrow(c_line)>1) {
    c_line<-sf::st_union(c_line) %>% sf::st_line_merge() %>% sf::st_sf()}
  
  #Spatial join with the dgo object to retrieve segment id
  c_line_id<-sf::st_intersection(c_line,dgo)
  
  #Create orthogonal lines for each segment
  
  o_lines<-list()
  
  for (i in 1:nrow(c_line_id)){
    o_lines[[i]]<-riverMapper::ortho_line(line = c_line_id[i,],line_length = max_width)}
  
  o_lines<-do.call(rbind,o_lines)
  
  #Intersect orthogonal lines with the dgo object to obtain ids and clip to the dgo boundary
  o_lines_dgo<-sf::st_intersection(o_lines,dgo)
  
  #Obtain corridor width
  corridor_width<-sf::st_length(o_lines_dgo)
  
  #Start computation of active channel width
  if (!is(water_mask)) {
    water_mask<-rast(water_mask)
  }
  
  #Extract the values along the orthogonal lines
  wm_olines<-terra::extract(water_mask,vect(o_lines))
  
  #Count flood (and thus assumed active channel) pixels along each line
  
  
  
  return(acw)
}