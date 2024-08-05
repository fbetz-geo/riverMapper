#'Extracting a channel network from a digital elevation network using SAGA GIS
#'@description This function provides a wrapper for two different approaches for channel network delineation from a digital elevation model
#'@param dem Path to a digital elevation model stored in a common raster format
#'@param preprocess option to use for preprocessing the DEM in order to create a depressionless elevation model      
#'                  can be "fill_sink" which is using the Wang and Liu sink filling algorithm or "breach" 
#'                  which is the SAGA implementation of breaching as in the Whitebox tools
#'@param min_slope minimum slope to maintain when carrying out sink filling; default is 0.1
#'@param initiation method for channel initiation, either "flow_acc" using flow accumulation or 
#'                  "cit" using the channel initiation index 
#'@param threshold threshold used for channel initiation. Commonly, flow accumulation is used for this.
#'@param min_seglength length (in cells) which a channel segment needs to have at least to be returned
#'@param saga_path path where the SAGA installation is found (see RSAGA::rsaga.env() for details)
#'@param out_dir directory where to store output files
#'@author Florian Betz
#'@return a list with a raster (terra rast() format) and a vector (terra vect() format) dataset of the channel network 
#'@export extract_channels_saga
#'

extract_channels_saga<-function(dem,preprocess="breach",min_slope=0.0001,initiation="cit",
                                threshold,min_seglength=10, saga_path,out_dir=tempdir()){
  
  #Set SAGA computation environment
  saga_env<-RSAGA::rsaga.env(saga_path)
  
  #Temporary set working directory
  dir<-getwd()
  setwd(out_dir)
  
  #DEM Preprocessing
  
  #Sink filling option
  if (preprocess=="fill_sink") {
    message("Preprocessing: filling sinks...")
    
    RSAGA::rsaga.geoprocessor(lib="ta_preprocessor",module="Fill Sinks XXL (Wang & Liu)",
                              param=list(ELEV=dem,
                                         FILLED="dem_preprocessed.sgrd",
                                         MINSLOPE=min_slope),
                              env = saga_env)
    
  }
  
  #Breaching option
  if (preprocess=="breach") {
    message("Preprocessing: breaching depressions...")
    
    RSAGA::rsaga.geoprocessor(lib="ta_preprocessor", module=7,
                              param = list(DEM=dem,
                                           NOSINKS="processed_dem"),
                              env = saga_env)
    
  }
  
  #Computing initiation grid
  if (initiation=="flow_acc") {
    message("Computing flow accumualation")
    
    RSAGA::rsaga.geoprocessor(lib="ta_hydrology",module="Flow Accumulation (Top-Down)",
                              param = list(ELEVATION=dem,
                                           FLOW="initiation_grid.sgrd",                                           
                                           METHOD=4,
                                           LINEAR_DO=1,
                                           LINEAR_MIN=500,
                                           CONVERGENCE=5),
                              env = saga_env)
    
  }
  
  if (initiation=="cit") {
    message("Computing channel initiation index")
    
    message("Computing catchment area...")
    RSAGA::rsaga.geoprocessor(lib="ta_hydrology",module="Flow Accumulation (Top-Down)",
                              param = list(ELEVATION=dem,
                                           FLOW="flow_acc.sgrd",                                           
                                           METHOD=4,
                                           LINEAR_DO=1,
                                           LINEAR_MIN=500,
                                           CONVERGENCE=5),
                              env = saga_env)
    
    message("Computing slope...")
    RSAGA::rsaga.geoprocessor(lib="ta_morphometry",module="Slope, Aspect, Curvature",
                              param = list(ELEVATION=dem,
                                            SLOPE="slope.srgd"),
                              env = saga_env)
    
    message("Computing CIT...")
    RSAGA::rsaga.geoprocessor(lib="ta_hydrology",module="CIT Index",
                              param = list(SLOPE="slope.sgrd",
                                           AREA="flow_acc.sgrd",
                                           CIT="initiation_grid.sgrd"),
                              env = saga_env)
    
  }
  
  
  #Extract Channel network
  message("Extracting channel network...")
  
  RSAGA::rsaga.geoprocessor(lib="ta_channels", module="Channel Network",
                            param = list(ELEVATION=dem,
                                         CHNLNTWRK="channel_grid.sgrd",
                                         SHAPES="channel_shape.gpkg",
                                         INIT_GRID="initiation_grid.sgrd",
                                         INIT_METHOD=2,
                                         INIT_VALUE=threshold,
                                         MINLEN=min_seglength),
                            env = saga_env)
  
  #Grab output channel raster and vector from the output folder and return in terra format
  l<-list(stream_rasters=terra::rast("channel_grid.sdat"),stream_vectors=terra::vect("channel_shape.gpkg"))
  
  return(l)
  
  #Reset the directory from out_dir to the original directory
  setwd(dir)
}