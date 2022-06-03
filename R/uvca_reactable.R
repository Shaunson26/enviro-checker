uvca_reactable <- function(uvca_resp){
  
  uvca_resp %>%
    select(label, value_text, value) %>%
    mutate(bar = value) %>%
    reactable::reactable(
      columns = list(
        label = colDef(name = 'Vegetation type'),
        value_text = colDef(name = 'Value category', maxWidth = 160),
        value = colDef(
          name = 'Percent cover',
          maxWidth = 110,
          align = 'right',
          cell = function(value, i){
            if (is.numeric(value)) {
              return(paste(round(value, 2), uvca_resp$unit[i]))
            }
            value
          }
        ),
        bar = colDef(
          name = '',
          align = 'left',
          cell = function(value,i) {
            
            bar <-
              div(style=list(width = uvca_resp$width[i],
                             backgroundColor = uvca_resp$background_colour[i]))
            
            div(style='width:100%;height:1em;display:flex;', bar)
            
            
          }
        )
      ))
}
