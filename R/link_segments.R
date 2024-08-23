#'Linking channel attributes and floodplain segments derived from the disaggregation function.
#'@description The link_segments() function takes the distances along the channel and assigns them to the disaggregated
#'floodplain segments
#'@param riverline sf object containing the riverline
#'@param segs sf object containing the floodplain segments as derived with the disaggregation function
#'@author Florian Betz
#'@return an sf object containing original floodplain segments with the distances along the channel
#'@export link_segments
#'

link_segments<-function(riverline,segs){
  
  #computing the downstream distance along the riverline
  dists<-riverMapper::profile_distance(riverline)
  
  #Spatial join with the floodplain segments and get median distance (i.e. the midpoint)
  seg_dists<-sf::st_join(dists,segs,left=TRUE) %>% dplyr::group_by(seg_id) %>% 
    summarize(seg_id=mean(seg_id),seg_dist=median(distances))
  
  #Link back to the original floodplain segments
  seg_dist_polys<-dplyr::left_join(segs,seg_dists,by="seg_id")
  
  return(seg_dist_polys)
  
}