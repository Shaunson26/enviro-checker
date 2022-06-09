create_mobile_results <- function(data){
  lapply(1:nrow(data), function(i) {
    div(
      class = 'mobile-result-item',
      p(data$label[i]),
      create_horizontal_bar_div(data[i, ]),
      hr(style = 'opacity:0.5;margin: 8px 0;')
    )
  }) %>%
    tagList()
}
