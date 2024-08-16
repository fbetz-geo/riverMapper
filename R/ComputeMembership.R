#' Compute Membership based on Linear Functions
#' @description Function to create linear fuzzy membership functions to delineate river corridor. The function can be either
#' linear or inverse linear (see details). 
#' @param index SpatRaster or path to raster file in any format readable by terra::rast()
#' @param x_cal_min numeric; lower bound of calibration range
#' @param x_cal_max numeric; upper bound of calibration range 
#' @param type character; can be either "linear" or "inv_linear"
#' @author Florian Betz
#' @references Betz, F., Lauermann, M., Cyffka, B. (2018): Delineation of the riparian zone in data-scarce regions
#' using fuzzy membership functions: An evaluation based on the case of the Naryn River in Kyrgyzstan. Geomorphology.
#' https://doi.org/10.1016/j.geomorph.2018.01.024
#' @return spatRaster containing the membership raster for the specific index
#' @export computeMembership
#' 

computeMembership<-function(index, x_cal_min,x_cal_max, type="linear"){
  #linear membership function
  if (type=="linear") {
    terra::app(index,function(x) {ifelse(x<x_cal_min,1,
           ifelse(x<x_cal_min,0,
                  ifelse(x>=x_cal_min & x<=x_cal_max,(x-x_cal_min)/(x_cal_max-x_cal_min),
                         ifelse(x>x_cal_max,1,
                                NA))))
                  })
    
  }
  
  #inverse linear membership functions
  if (type=="inv_linear") {
    terra::app(index,function(x) {ifelse(x<=x_cal_max,1-(x/x_cal_max),
           ifelse(x>x_cal_max,0,
                  NA))})
    
  }
}
