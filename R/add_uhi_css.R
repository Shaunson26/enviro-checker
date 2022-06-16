add_uhi_css <- function(x){
  
  redColorRamp <-
    colorRamp(RColorBrewer::brewer.pal(9, 'Reds'))
  
  blueColorRamp <-
    colorRamp(RColorBrewer::brewer.pal(9, 'Blues'))
  
  if (is.numeric(x$value)) {
    
    value = ifelse(x$value > 9, 9,  x$value)
    
    if (value < 0){
      value = -1 * value
      x$background_colour <- rgb(blueColorRamp(value/9), maxColorValue=255)
    } else {
      x$background_colour <- rgb(redColorRamp(value/9), maxColorValue=255)
    }
    x$width <- paste0(round(value/9 * 100, 0), "%")
    
  } else {
    
    x$background_colour <- rgb(redColorRamp(0/9), maxColorValue=255)
    x$width <- paste0(0, "%")
    
  }
  
  x$unit <- "°C"
  
  x
  
}
