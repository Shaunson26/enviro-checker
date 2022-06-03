#' Query the flat minified GNAF dataset
#'
#' A filter dplyr::filter function on the columns in \code{gnaf}. Missing values
#' will return a T in the filter step, essentially meaning 'everything' for that
#' column
#'
#' @param street_number street number without any unit/apartment info
#' @param street_name street name
#' @param locality locality/suburb
#' @param postcode postcode. Must be 4 digits
#'
#' @export
return_address_coords <- function(street_number, street_name, locality, postcode){

  # TODO missing to ''

  # Input checking ----

  # stopifnot(
  #   'street_number cannot have characters' = !grepl('\\D', street_number),
  #   'postcode cannot have characters' = !grepl('\\D', postcode),
  #   'postcode must be 4 digits in length' = nchar(postcode) == 4
  # )
  #

  stopifnot(
    'street_number must be a number' = ifelse(missing(street_number), TRUE, is.numeric(street_number)),
    'street_name must be a character' = ifelse(missing(street_name), TRUE, is.character(street_name)),
    'street_number cannot have characters' = ifelse(missing(street_number), TRUE, !grepl('\\D', street_number)),
    'locality must be a character' = ifelse(missing(locality), TRUE, is.character(locality)),
    # "postcode must be a number" =  ifelse(missing(postcode), TRUE, is.numeric(postcode)),
    'postcode cannot have characters' = ifelse(missing(postcode), TRUE, !grepl('\\D', postcode)),
    'postcode must be 4 digits in length' = ifelse(missing(postcode), TRUE, nchar(postcode) == 4)
  )

  # Function ----
  if (!missing(postcode)){
    postcode <- as.numeric(postcode)
  }

  if (!missing(street_number)){
    street_number <- as.numeric(street_number)
  }

  missing_street_number = ifelse(missing(street_number), TRUE, FALSE)
  missing_street_name = ifelse(missing(street_name), TRUE, FALSE)
  missing_locality = ifelse(missing(locality), TRUE, FALSE)
  missing_postcode = ifelse(missing(postcode), TRUE, FALSE)

  # [to-do] to upper, partial matching

  dplyr::filter(gnaf,
                if (missing_street_number) T else NUMBER_FIRST == street_number,
                if (missing_street_name) T else STREET_NAME == street_name,
                if (missing_locality) T else LOCALITY_NAME == locality,
                if (missing_postcode) T else POSTCODE == postcode)
}



