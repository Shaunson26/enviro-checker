#' Create unique vector including NA
#' 
#' @param x a vector
#' 
#' @return a vector
unique_input_values <- function(x) {
  c(NA, sort(unique(x)))
}