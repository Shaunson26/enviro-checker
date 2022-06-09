filter_gnaf <- function(x, inputs, test = FALSE){
  
  if (test) {
    input <-
      list(number = 37,
           street = 'ST PAULS CRESCENT',
           suburb = 'LIVERPOOL',
           postcode = 2170)
    x <- gnaf
  }
  
  x %>%
    filter(
      if (inputs$number == '')
        T
      else
        NUMBER_FIRST == as.integer(inputs$number),
      if (inputs$street == '')
        T
      else
        STREET_NAME == inputs$street ,
      if (inputs$suburb == '')
        T
      else
        LOCALITY_NAME == inputs$suburb ,
      if (inputs$postcode == '')
        T
      else
        POSTCODE == as.integer(inputs$postcode)
    )
}
