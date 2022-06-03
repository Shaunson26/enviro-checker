uhi_reactable <- function(uhi_resp) {
  
  value_unit <- function(value) {
    if (is.numeric(value)) {
      return(paste(round(value, 2), uhi_resp$unit))
    }
    value
  }
  
  bar_div <- function(value) {
    bar <-
      div(style = list(
        width = uhi_resp$width,
        backgroundColor = uhi_resp$background_colour
      ))
    
    div(style = 'width:100%;height:1em;display:flex;', bar)
    
  }
  
  uhi_resp %>%
    select(label, value_text, value, ) %>%
    mutate(label = 'Urban Heat Island') %>% 
    mutate(bar = value) %>%
    reactable::reactable(columns = list(
      label = colDef(name = 'Measure'),
      value_text = colDef(name = 'Value category', maxWidth = 160),
      value = colDef(
        name = 'Value',
        maxWidth = 110,
        align = 'right',
        cell = value_unit
      ),
      bar = colDef(
        name = '',
        align = 'left',
        cell = bar_div
      )
    ))
  
}
