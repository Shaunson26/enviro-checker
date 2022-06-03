create_horizontal_bar_div <- function(result, minText = 'min', maxText = 'max'){
  
  backgroundColor = paste0("background-color:", result$background_colour , ';')
  width = paste0("width:", result$width, ';')
  
  value_unit <- function(value) {
    if (is.numeric(value)) {
      return(paste(round(value, 2), result$unit))
    }
    value
  }
  
  tags$div(id='mobile-result-bar',
    style='display: flex;width: 100%;align-items: center;',
    #tags$span(minText, style = 'padding: 4px;'),
    tags$div(
      style="width: 100%;display: flex;align-items: flex-end;",
      tags$div(
        style=paste('height: 1.25em;', width, backgroundColor),
      ),
      tags$span(
        style ='padding: 0 6px;white-space: nowrap;',
        value_unit(result$value)
      )
    )
    #,tags$span(maxText, style= 'padding: 4px;')
  )
  
}