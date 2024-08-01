#' Helper function to install GRASS GIS on the machine
#' @description This function downloads the GRASS GIS installer from the web and initiates the installation
#' @param url download url of GRASS GIS; it defaults to GRASS 8.4.0, the latest version at the time of writing this function
#' @param download_folder folder where to download the GRASS installer file, typically your download folder
#' @author Florian Betz
#' @return This function doesn't return any object
#' @export install_grass
#' 

install_grass<-function(url="https://grass.osgeo.org/grass84/binary/mswindows/native/WinGRASS-8.4.0-1-Setup.exe",
                        download_folder="C:/Downloads"){
  
  #Checking for the operating system
  os<-Sys.info()["sysname"]
  
  if (os!="Windows") {
    stop("Currently, only Windows is supported for this function. 
         See the GRASS GIS website (https://grass.osgeo.org/download/) for more information how 
         to install GRASS GIS on your computer")}
  
  
  #Create destination file name
  destination<-paste(download_folder,"WinGRASS-8.4.0-1-Setup.exe",sep = "/")
  
  
  #Increase timeout time to ensure complete download, 10 minutes should be fine even for slow connections
  options(timeout=600)
  
  #Download the GRASS installer
  utils::download.file(url=url,destfile = download_path,mode="wb",method = "libcurl")
  
  system("start",destination)
  
}