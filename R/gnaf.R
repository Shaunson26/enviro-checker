#' Flat GNAF address dataset
#'
#' Geoscape G-NAF is Australia’s authoritative, geocoded address file. It is built
#' and maintained by Geoscape Australia using authoritative government data.
#'
#' G-NAF is one of the most ubiquitous and powerful spatial datasets. It contains
#' more than 14 million Australian physical address records. The records include
#' geocodes, which are latitude and longitude map coordinates. G-NAF does not
#' contain personal names.
#'
#' This dataset was originally found on data.gov.au
#'
#' \url{https://data.gov.au/data/dataset/19432f89-dc3a-4ef3-b943-5326ef1dbecc}
#'
#' The data was flattened from the original set of data files and only includes
#' the main street address (Units/apartments are not included), coordinates and meshblock 2016
#'
#' @format A data frame with 2770292 rows and 6 variables:
#' \describe{
#'   \item{NUMBER_FIRST}{int, street number}
#'   \item{STREET_NAME}{chr, street name}
#'   \item{LOCALITY_NAME}{chr, locality or suburb name}
#'   \item{POSTCODE}{int, postcode}
#'   \item{LONGITUDE}{num, longitude of address}
#'   \item{LATITUDE}{num, latitude of address}
#'   \item{MB_2016_CODE}{char, meshblock 2016 code}
#' }
#' @source \url{https://data.gov.au}
'gnaf'
