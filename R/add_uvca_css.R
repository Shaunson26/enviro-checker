add_uvca_css <- function(x){
  
  greenColorRamp <-
    colorRamp(RColorBrewer::brewer.pal(9, 'Greens')[-c(1:4)])
  
  if (is.numeric(x$value)) {
    
    x$background_colour <- rgb(greenColorRamp(x$value/100), maxColorValue=255)
    # 40 % or greater is high, stretch values a bit, 75% max?
    max_value = 75
    x$width <- paste0(round(x$value / max_value * 100, 1), "%")
    
  } else {
    
    x$background_colour <- rgb(greenColorRamp(0/100), maxColorValue=255)
    x$width <- paste0(0, "%")
    
  }

  x$unit <- '%'
  
  x
  
}
