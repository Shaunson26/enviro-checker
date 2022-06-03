hvi_reactable <- function(hvi_resp){
  
  hvi_resp %>%
    select(label, value_text, value) %>%
    mutate(bar = value) %>%
    reactable::reactable(
      columns = list(
        label = colDef(name = 'Index'),
        value_text = colDef(name = 'Index category', maxWidth = 160),
        value = colDef(
          name = 'Value',
          maxWidth = 110,
          align = 'right'),
        bar = colDef(
          name = '',
          align = 'left',
          cell = function(value, i) {
            
            bar <-
              div(style= list(width =  hvi_resp$width[i],
                              backgroundColor =  hvi_resp$background_colour[i]))
            
            div(style='width:100%;height:1em;display:flex;', bar)
          }
        )
      )
    )
}
