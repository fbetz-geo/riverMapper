#'Little helper function to find the mode in a character vector
#'@description This function finds the entrance with the highest frequency in a character vector, i.e. the mode
#'@param x character vector
#'@author Florian Betz
#'@return single character with the most frequent entrance in x
#'@export find_mode
#'
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}