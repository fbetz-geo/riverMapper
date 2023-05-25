#' Compute Morphometric Indices
#' @description Computes different morphometric indices useful for the fuzzy delineation of river corridors
#' @param dem path to the digital elevation model
#' @param channels path to raster of channel network delineated from digital elevation models
#' @param index character; indices to be computed, can be "hdist" for horizontal distance, "vdist"   
#' for vertical distance, "mrvbf" for multiresolution valleybottom flatness index, "TWI" for Wetness Index
#' or "PathDist" for Path Distance
#' @param saga_path path where the SAGA installation is found (see RSAGA::rsaga.env() for details)
#' @param out_dir directory where to store output files
#' @author Florian Betz
#' @references Betz, F., Lauermann, M., Cyffka, B. (2018): Delineation of riparian zone...
#' @return spatRaster with the morphometric indices
#' 

computeIndices<-function(dem,channels,saga_path, out_dir=tempdir()){
  
  #Set SAGA computation environment
  saga_env<-RSAGA::rsaga.env(saga_path)
  
  #Temporary set working directory
  dir<-getwd()
  setwd(out_dir)
  
  #Compute vertical distance to channel network
  message("Computing Vertical Distance to Channel Network...")
  RSAGA::rsaga.geoprocessor(lib = "ta_channels", module = "Vertical Distance to Channel Network",
                     param = list(ELEVATION = dem, 
                                  CHANNELS= channels, 
                                  DISTANCE= paste(out_dir,"vdist.sgrd",sep="/")),
                     env = saga_env)
    
  #Compute horizontal distance
  message("Computing Horizontal Distance to Channel Network...")
  RSAGA::rsaga.geoprocessor(lib="grid_tools",module = "Proximity Grid",
                            param = list(FEATURES=channels,
                                         DISTANCE=paste(out_dir,"hdist.sgrd",sep="/")),
                            env = saga_env)
    
  #Compute wetness index
  message("Computing SAGA Wetness Index...")
  RSAGA::rsaga.geoprocessor(lib="ta_hydrology",module = "SAGA Wetness Index",
                            param = list(DEM=dem,
                                         TWI=paste(out_dir,"twi.sgrd",sep="/")),
                            env = saga_env)
    
  #Compute multiresolution valley bottom flatness index
  message("Computing Multiresolution Valley Bottom Flatness Index...")
  RSAGA::rsaga.geoprocessor(lib="ta_morphometry",module = "Multiresolution Index of Valley Bottom Flatness (MRVBF)",
                            param = list(DEM=dem,
                                         MRVBF=paste(out_dir,"mrvbf.sgrd",sep="/")),
                            env = saga_env)
  
  #Stacking inputs
  message("Stacking Outputs...")
  indices<-rast("vdist.sgrd","hdist.sgrd","twi.sgrd","mrvbf.sgrd")
  names(indices)<-c("Vertical Distance to Channel Network","Horizontal Distance to Channel Network",
                    "Topographic Wetness Index","Multiresolution Vallex Bottom Flatness Index")
  
  return(indices)
  
  #Reset working directory to original directory
  setwd(dir)
} 