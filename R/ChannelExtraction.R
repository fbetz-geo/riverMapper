#'Extracting a channel network from a digital elevation network using SAGA GIS
#'@description This function provides a wrapper for two different approaches for channel network delineation from a digital elevation model
#'@param dem Path to a digital elevation model stored in a common raster format
#'@param preprocess option to use for preprocessing the DEM in order to create a depressionless elevation model      
#'                  can be "fill_sink" which is using the Wang and Liu sink filling algorithm or "breach" 
#'                  which is the SAGA implementation of breaching as in the Whitebox tools
#'@param processed_dem output file name of the processed dem; it will be written to out_dir
#'@param min_slope minimum slope to maintain when carrying out sink filling; default is 0.1
#'@param initiation method for channel initiation, either "flow_acc" using flow accumulation or 
#'                  "cit" using the channel initiation index 
#'@param thresh threshold used for channel initiation. Commonly, flow accumulation is used for this.
#'@param saga_path path where the SAGA installation is found (see RSAGA::rsaga.env() for details)
#'@param out_dir directory where to store output files
#'@param overwrite should already existing files be overwritten? Defaults to true and only applies to the pathDistance!
#'@author Florian Betz
#'@return a raster and a vector dataset of the channel network

channelExtraction<-function(dem,preprocess="breach",processed_dem,min_slope=0.1,initiation="cit",thresh,saga_path,out_dir=tempdir(),overwrite=FALSE){
  
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
                                         FILLED=processed_dem,
                                         MINSLOPE=min_slope),
                              env = saga_env)
    
  }
  
  #Breaching option
  if (preprocess=="breach") {
    message("Preprocessing: breaching depressions...")
    
    RSAGA::rsaga.geoprocessor(lib="ta_preprocessor", module="Breach Depressions",
                              param = list(DEM=dem,
                                           NOSINKS=processed_dem),
                              env = saga_env)
    
  }
  
  #Computing initiation grid
  if (initiation=="flow_acc") {
    message("Computing flow accumualation")
    
    RSAGA::rsaga.geoprocessor(lib="ta_hydrology",module="Flow Accumulation (Top-Down)",
                              param = list(ELEVATION=dem,
                                           FLOW="initiation_grid.sgrd",                                           METHOD=4,
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
                                           FLOW="flow_acc.sgrd",                                           METHOD=4,
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
                                         INIT_VALUE=thresh),
                            env = saga_env)
  
  
}