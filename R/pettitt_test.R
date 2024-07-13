#'Test for changepoints in an univariate series using an iterative pettitt test
#'@description This function applies the commonly used pettitt test in an iterative way, where new segments 
#'are created by a changepoint and searched for new changepoints until no significant result of the pettitt
#' test is returned anymore.
#'@param data vector of continuous data to be segmented
#'@param alpha significance level to be used in the pettitt test
#'@author Florian Betz
#'@return vector of changepoint locations in the input data
#'@export pettitt_test
#'


pettitt_test <- function(data, alpha = 0.05) {
  changepoints <- list()
  segments <- list(list(data = data, start = 1, end = length(data)))
  
  while (length(segments) > 0) {
    
    # Get the first segment to process
    segment_info <- segments[[1]]
    segments <- segments[-1]
    
    segment <- segment_info$data
    start_idx <- segment_info$start
    end_idx <- segment_info$end
    
    # Ensure there is enough data for the test
    if (length(segment) > 1) { 
      
      # Apply the Pettitt test
      pettitt_result <- trend::pettitt.test(segment)
      
      # Check if the result is significant
      if (pettitt_result$p.value < alpha) {
        
        # Get the position of the changepoint relative to the current segment
        cp <- pettitt_result$estimate
        changepoint_global_idx <- start_idx + cp - 1
        changepoints <- c(changepoints, changepoint_global_idx)
        
        # Split the data at the changepoint and add to new segments
        segments <- c(
          segments, 
          list(list(data = segment[1:cp], start = start_idx, end = start_idx + cp - 1)),
          list(list(data = segment[(cp+1):length(segment)], start = start_idx + cp, end = end_idx))
        )
      }
    }
  }
  
  return(unname(sort(unlist(changepoints))))
}
