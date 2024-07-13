#' Aggregation of the disaggregated units to uniform reaches using statistical methods
#' @description
#' This function provides various methods to aggregate the disaggregated river corridor into homogeneous reaches.
#' It provides the univariate Pettitt test, a multivariate Bayesian method as well as a multivariate clustering method
#' @param river_df data.frame with the river corridor attributes on the disaggregated level.
#' @param method can be one of the following: pettitt for the univariate pettitt test
#' @param variable variable from river_df to conduct the univariate pettitt segmentation
#' @param alpha significance level of the pettitt test used to decide whether there is still 
#' a significant changepoint in a segment and segmentation should be continued
#' @author Florian Betz
#' @return a new data.frame including all column from river_df with an additional column with index values for each segment
#'@export aggregation

aggregation<-function(river_df,method="pettitt", variable, alpha=0.05)
{

  #Pettitt test (univariate changepoint method)
  if (method=="pettitt") {
    
    
  }
}