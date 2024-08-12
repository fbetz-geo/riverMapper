#' Compute Morphometric Indices
#' @description Computes different morphometric indices useful for the fuzzy delineation of river corridors
#' @param dem path to the digital elevation model
#' @param channels path to raster of channel network delineated from digital elevation models
#' @param index character; indices to be computed, can be "hdist" for horizontal distance, "vdist"   
#' for vertical distance, "mrvbf" for multiresolution valleybottom flatness index, "TWI" for Wetness Index
#' or "PathDist" for Path Distance
#' @param gis_backend which GIS software to use for computing the more complex terrain parameters; 
#' currently only SAGA GIS is supported with GRASS GIS being available soon
#' @param gis_path path where the SAGA installation is found (see RSAGA::rsaga.env() for details)
#' @param out_dir directory where to store output files
#' @param overwrite should already existing files be overwritten? Defaults to true and only applies to the pathDistance!
#' @author Florian Betz
#' @references Betz, F., Lauermann, M., Cyffka, B. (2018): Delineation of riparian zone...
#' @details Vertical Distance, and horizontal distance are currently computed using SAGA GIS. The computation is efficient, but not memory safe,    
#' #' i.e. the entire DEM has to fit in RAM. Thus, it is recommended to watch memory use while computing these indices. 
#' @return spatRaster with the morphometric indices as layers
#' @export computeIndices
#' 

computeIndices<-function(dem,channels,gis_backend="saga", gis_path=c("vdist","PathDist","mrvbf"), index,out_dir=tempdir(),overwrite=FALSE){
  
  #Check for GIS backend
  if (gis_backend=="saga") {
    
  #Set SAGA computation environment
  saga_env<-RSAGA::rsaga.env(gis_path)
  
  #Temporary set working directory
  dir<-getwd()
  setwd(out_dir)
  
  #Preparing empty list for the output rasters
  stack_list<-list()
  
  #Compute vertical distance to channel network
  if ("vist" %in% index) {
    message("Computing Vertical Distance to Channel Network...")
    RSAGA::rsaga.geoprocessor(lib = "ta_channels", module = "Vertical Distance to Channel Network",
                     param = list(ELEVATION = dem, 
                                  CHANNELS= channels, 
                                  DISTANCE= paste(out_dir,"vdist.sgrd",sep="/")),
                     env = saga_env)
    
    #Adding result to stack list for the final output
    stack_list[["vdist"]]<-rast("vdist.sdat")}
    
  #Compute horizontal distance
  if ("hdist" %in% index) {
    message("Computing Horizontal Distance to Channel Network...")
    RSAGA::rsaga.geoprocessor(lib="grid_tools",module = "Proximity Grid",
                            param = list(FEATURES=channels,
                                         DISTANCE=paste(out_dir,"hdist.sgrd",sep="/")),
                            env = saga_env)
    
    #Adding result to stack list for the final output
    stack_list[["hdist"]]<-rast("hdist.sdat")}
  
  #Compute path distance
  if ("pathDist" %in% index) {
    message("Computing path distance: preparing slope raster step 1")
    
    #compute slope as basis for the path distance
    slope<-terra::terrain(x=dem, filename="slope.tif")
    
    #Read channels, stack with slope raster and set proper names
    channels<-rast(channels)
    s<-c(slope,channels)
    names(s)<-c("slope","channels")
    
    #Prepare slope raster for cost distance computation by setting target value for stream locations
    message("Computing path distance: preparing slope raster step 2")
    slope_pathDist<-terra::ifel(is.na(s["channels"]),s["slope"],-1000)
    
    #Computing path distance
    message("Computing path Distance: Computing cumulated cost distance...")
    pathDist<-terra::costDist(slope_pathDist,target=-1000,filename="pathDist.tif",overwrite=overwrite)
    
    #Adding result to stack list for the final output
    stack_list[["pathDist"]]<-rast("pathDist.tif")}
    
  #Compute wetness index
  if ("TWI" %in% index) {
    message("Computing SAGA Wetness Index...")
    RSAGA::rsaga.geoprocessor(lib="ta_hydrology",module = "SAGA Wetness Index",
                            param = list(DEM=dem,
                                         TWI=paste(out_dir,"twi.sgrd",sep="/")),
                            env = saga_env)
    
    #Adding result to stack list for the final output
    stack_list[["twi"]]<-rast("twi.sdat")}
    
  #Compute multiresolution valley bottom flatness index
  if ("mrvbf" %in% index) {
    message("Computing Multiresolution Valley Bottom Flatness Index...")
    RSAGA::rsaga.geoprocessor(lib="ta_morphometry",module = "Multiresolution Index of Valley Bottom Flatness (MRVBF)",
                            param = list(DEM=dem,
                                         MRVBF=paste(out_dir,"mrvbf.sgrd",sep="/")),
                            env = saga_env)
    
    #Adding result to stack list for the final output
    stack_list[["mrvbf"]]<-rast("mrvbf.sdat")}
  }
  
  #Stacking inputs
  message("Stacking outputs...")
  indices<-rast(stack_list)
  
  #Return final result as spatRaster 
  return(indices)
  
  #Reset working directory to original directory
  setwd(dir)
} 
