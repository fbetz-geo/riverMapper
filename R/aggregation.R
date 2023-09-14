#' Aggregation of the disaggregated units to uniform reaches using statistical methods
#' @description
#' This function provides various methods to aggregate the disaggregated river corridor into homogeneous reaches.
#' It provides the univariate Pettitt test, a multivariate Bayesian method as well as a multivariate clustering method
#' @param river_df data.frame with the river corridor attributes on the disaggregated level.
#' @param method can be one of the following: pettitt for the univariate pettitt test, cluster for clustering or
#' bcp for Bayesian changepoint estimation; default is "bcp"
#' @param thresh threshold for the changepoint probability to be used by the bcp method
#' 
aggregation<-function(river_df,method="bcp", thresh)
{
  #Bayesian changepoint method
  if (method=="bcp") {
    m<-as.matrix(river_df)
    b<-bcp::bcp(m)
    
  }
  
  #Pettitt test (univariate changepoint method)
  if (method=="pettitt") {
    
    
  }
}