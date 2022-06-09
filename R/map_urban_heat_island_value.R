map_urban_heat_island_value <- function(x){

  attributes_labels <-
    c('UHI_16_m' = 'Urban heat island')

  value_categories <-
    c('Cooler than baseline', '0-3 degrees warmer', '3-6 degrees warmer',
      '6-9 degrees warmer', '>9 degrees warmer')

  categorise_value <- function(x) {
    cut(x, c(-Inf, 0,3,6,9, Inf), labels = value_categories)
  }

  if (length(x$features) == 0){

    message('Address is outside the extent of data. No data is within the features element')

    tibble::tibble(label = unname(attributes_labels[1]),
                   value = 'no data',
                   value_text = 'no data')

  } else {

    tibble::tibble(label = unname(attributes_labels[1]),
                   value = x$features[[1]]$attributes[[names(attributes_labels)]],
                   value_text = categorise_value(value))

  }

}
