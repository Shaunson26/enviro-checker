filter_address <- function(data, number, street, suburb, postcode) {
  data %>%
    dplyr::filter(
      POSTCODE == as.integer(postcode),
      LOCALITY_NAME == suburb,
      STREET_NAME == street,
      NUMBER_FIRST == as.integer(number)
    )
}
