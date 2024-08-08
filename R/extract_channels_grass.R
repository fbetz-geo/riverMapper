#'Extracting a channel network from a digital elevation network using GRASS GIS
#'@description This function provides a wrapper for two different approaches for channel network delineation from a digital elevation model
#'@param dem Path to a digital elevation model stored in a common raster format or terra SpatRaster
#'@param preprocess_dem should the DEM be hydrologically conditioned before analysis? If TRUE, a minimum impact
#' DEM treatment using the procedure of Lindsay and Creed (2005) is carried out.
#'@param grass_path path where the GRASS installation is found (see rgrass::initGRASS for details)
#'@param grass_db directory where to create the GRASS database; if not provided, tempdir() is used instead
#'@param basin_threshold integer; minimum size of exterior watershed basin (see the help of r.watershed for more details)
#'@param channel_threshold flow accumulation threshold for initiating channels; if montgomery is>0, the channel initiation    
#'threshold method according to Montgomery and Foufoula-Georgiou (1993) is used instead of flow accumulation
#'@param montgomery exponent for adjusting flow accumulation to slope
#'@param mfd_threshold flow accumulation threshold where to switch from multiple flow direction to single flow direction 
#' for channel derivation; setting mfd_threshold to 0 (the default) disables multiple flow direction completely
#'@param min_seglength length (in cells) which a channel segment needs to have at least to be returned
#'@param compute_orders logical; if TRUE, commonly used stream orders will be computed using the r.stream.order addon of GRASS GIS
#'@param stream_orders character vector of the stream orders to return in the stream raster output; 
#'can be any strahler, horton, hack, shreve, topo; by GRASS GIS default, the stream vector output will automatically include all orders as attributes
#'@param disk_swap logical; if FALSE, then the computation will be carried out all in RAM; if TRUE, disk swap calculation
#'is used consuming a maximum amount of RAM is specified in memory
#'@param memory maximum RAM to use for disk swap computation, default is 300
#'@param remove logical; if TRUE, the temporary GRASS database will be removed when quitting the R session or unloading rgrass
#'@author Florian Betz
#'@references Lindsay, J. B., and Creed, I. F. 2005. Removal of artifact depressions from digital elevation models: towards a minimum impact approach. 
#'Hydrological Processes 19, 3113-3126. DOI: 10.1002/hyp.5835 
#'@return a list with a raster (terra rast() format) and a vector (sf() format) dataset of the channel network 
#'@export extract_channels_grass
#'

extract_channels_grass<-function(dem,preprocess_dem=FALSE, grass_path,grass_db,basin_threshold, channel_threshold=10000,
                                 montgomery=0, mfd_threshold=0, min_seglenth,compute_orders=TRUE,stream_orders=c("hack","strahler"),
                                 disk_swap=FALSE, memory=300,remove=TRUE){
  
  #check if dem is already terra spatRaster
  if (!inherits(dem,"SpatRaster")) {
    
    #Convert to SpatRaster
    dem<-rast(dem)
    }
  
  #Initialize GRASS session
  rgrass::initGRASS(gisBase = grass_path,
                    SG=dem,
                    gisDbase = grass_db,
                    remove_GISRC = remove)
  
  #Read the DEM to the GRASS database
  rgrass::write_RAST(dem,"dem",flags = "overwrite")
  
  
  #Preprocess DEM if required
  if (preprocess_dem) {
    #check if r.stream.orders addon is installed
    addons<-rgrass::execGRASS("g.extension",flags="a",intern = TRUE)
    if (!"r.hydrodem" %in% addons) {
      message("r.hydrodem addon is not installed. Installing it with g.extension")
      
      #Install r.stream.order addon if not found
      rgrass::execGRASS("g.extension",extension="r.hydrodem",operation="add")}
    
    rgrass::execGRASS("r.hydrodem",parameters = list(input="dem",
                                                     output="dem_hydrodem",
                                                     memory=memory))
    #Set the name of the DEM to use in further computation, here the preprocessed DEM
    dem_file="dem_hydrodem"
    
  }else{
    
    #Or set to the original DEM if preprocessing was not requested
    dem_file="dem"
  }
  
  
  #Compute flow accumulation and drainage direction using r.watershed
  
  #Set flag if disk swap computation is requested
  w_flags<-c()
    if (disk_swap) {
      w_flags<-c(flags,"m")}
  
  #execute the GRASS module
  rgrass::execGRASS("r.watershed", parameters = list(elevation=dem_file,
                                                     threshold=basin_treshold,
                                                     accumulation="accumulation",
                                                     drainage="drain_dir",
                                                     memory=memory,
                                                     flags=w_flags))
  
  
  #Execute channel extraction using r.stream.extract
  rgrass::execGRASS("r.stream.extract", parameters = list(elevation=dem_file,
                                                          acumulation="accumulation",
                                                          threshold=channel_threshold,
                                                          mexp=montgomery,
                                                          d8cut=mfd_threshold,
                                                          stream_length=min_seglength,
                                                          stream_raster="channels",
                                                          stream_vector="channels",
                                                          direction="draindir_extract"))
  
  
  #Compute stream orders if requested
  if (compute_orders) {
    
    #check if r.stream.orders addon is installed
    addons<-rgrass::execGRASS("g.extension",flags="a",intern = TRUE)
    if (!"r.stream.order" %in% addons) {
      message("r.stream.order addon is not installed. Installing it with g.extension")
      
      #Install r.stream.order addon if not found
      rgrass::execGRASS("g.extension",extension="r.stream.order",operation="add")}
      
      #Execute the module
      rgrass::execGRASS("r.stream.order",parameters = list(stream_rast="channels",
                                                           direction="draindir_extract",
                                                           elevation=dem_file,
                                                           accumulation="accumulations",
                                                           stream_vect="stream_orders",
                                                           hack="hack",
                                                           strahler="strahler",
                                                           horton="horton",
                                                           shreve="shreve",
                                                           topo="topo",
                                                           memory=memory))
      
  }
  
  #Get the results back from GRASS to the R session
  if (compute_orders) {
    
    #Get SpatRaster object of the stream orders
    stream_raster<-rgrass::read_RAST(stream_orders)
    
    #Get vector object of the stream orders
    stream_vector<-rgrass::read_VECT("stream_orders",type="line")
  } else{
    
    #Get the results of the computation without orders
    
    #Get SpatRaster of stream raster
    stream_raster<-rgrass::read_RAST("channels")
    
    #Get vector of the channels
    stream_vector<-rgrass::read_VECT("channels",type="line")
    
  }
  
  #Create the list for storing the output object
  l<-list()
  
  l$stream_raster<-stream_raster
  l$stream_vector<-stream_vector
  
  return(l)
  
}