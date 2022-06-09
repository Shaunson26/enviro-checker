create_address_text <- function(x){
  address_cols <-
    c('NUMBER_FIRST',
      'STREET_NAME',
      'LOCALITY_NAME',
      'POSTCODE')
  paste(unlist(x[, address_cols]), collapse = ' ')
}