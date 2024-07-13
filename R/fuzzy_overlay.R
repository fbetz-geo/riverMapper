#' Overlay several fuzzified indices
#' @description Combining several fuzzified indices using defined Zadeh's classic AND/OR rules
#' @param x list of fuzzified indices
#' @param mode mode of overlay, either "AND" or "OR"
#' @author Florian Betz
#' @references Betz, F., Lauermann, M., Cyffka, B. (2018):
#' @return spatRaster of a value range from 0-1 where 1 is fully connected to the channel and 0 is not connected at all.
#' @export fuzzy_overlay
#' 

fuzzy_overlay<-function(x,mode="AND"){
  stck<-rast(x)
  
  #AND mode computed as minimum of the stack
  if (mode=="AND") {
    r<-terra::app(stck,min,na.rm=TRUE)
    return(r)
  }
  
  #OR mode computed as maximum of the stack
  if (mode=="OR") {
    r<-terra::app(stck,max,na.rm=TRUE)
    return(r)
    
  }
}

