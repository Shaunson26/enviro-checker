add_hvi_css <- function(x){
  
  colours <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  
  x$width <- paste0(x$value * 100 / 5, "%")

  x$background_colour <-
    ifelse(grepl('Adaptive', x$label),
           rev(colours)[x$value],
           colours[x$value])
  
  x$unit <- ''
  
  x
  
}
