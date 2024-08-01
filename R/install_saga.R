#' Helper function to install SAGA GIS on the machine
#' @description This function downloads the SAGA GIS installer from the web and initiates the installation
#' @param url download url of SAGA GIS; it defaults to SAGA 9.0.1 as this is the version riverMapper has been tested with
#' @param install_folder directory, where SAGA will be living. This is not necessarily C:/Program Files
#' @author Florian Betz
#' @return This function doesn't return any object
#' @export install_saga
#' 

install_saga<-function(url="https://sourceforge.net/projects/saga-gis/files/SAGA%20-%208/SAGA%20-%208.4.1/saga-8.4.1_x64.zip/download",
                       install_folder="C:/SAGA"){
  
  #Checking for the operating system
  os<-Sys.info()["sysname"]
  
  if (os!="Windows") {
    stop("Currently, only Windows is supported for this function. 
         See the SAGA website (https://saga-gis.sourceforge.io/) for more information how 
         to install SAGA GIS on your computer")}
    
    
    #Download the zipfile with SAGA files
    download_path<-paste(install_folder,"saga.zip",sep="/")
    
    #Increase timeout time to ensure complete download, 10 minutes should be fine even for slow connections
    options(timeout=600)
    utils::download.file(url=url,destfile = download_path,mode="wb",method = "libcurl")
    
    
    #Extracting the files into the install_folder
    message("Extracting SAGA files")
    utils::unzip(zipfile = download_path,exdir = install_folder)
    
    message(paste0("SAGA successfully extracted to ", install_folder))
            
  
  #Remove zip-file
  unlink(download_path)
}
