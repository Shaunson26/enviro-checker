add_uhi_css <- function(x){
  
  redColorRamp <-
    colorRamp(RColorBrewer::brewer.pal(9, 'Reds'))
  
  if (is.numeric(x$value)) {
    
    value_max = ifelse(x$value > 9, 9,  x$value)
    x$background_colour <- rgb(redColorRamp(value_max/9), maxColorValue=255)
    x$width <- paste0(round(value_max/9 * 100, 0), "%")
    
  } else {
    
    x$background_colour <- rgb(redColorRamp(0/9), maxColorValue=255)
    x$width <- paste0(0, "%")
    
  }
  
  x$unit <- "°C"
  
  x
  
}
