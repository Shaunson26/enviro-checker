#' Get Urban Heat Island
#'
#' The Urban Heat Island (UHI) dataset measures the effects of urbanisation on land
#' surface temperatures across Sydney Greater Metropolitan Area for the Summer of 2015-2016.
#' UHI shows the variation of temperature to a non-urban vegetated reference, such as
#' heavily wooded areas or national parks around Sydney. Derived from the analysis of
#' thermal and infrared data from Landsat satellite, the dataset has been combined
#' with the Australian Bureau of Statistics (ABS) Mesh Block polygon dataset to
#' provide a mean UHI temperature that enables multi-scale spatial analysis of
#' the relationship of heat to green cover.
#'
#' https://datasets.seed.nsw.gov.au/dataset/nsw-urban-heat-island-to-modified-mesh-block-2016
#'
#' @param mb text, Mesh block 2016 code
#'
#' @export
get_urban_heat_island_value <- function(mb, returnGeometry = FALSE){

  # stopifnot(
  #   'Only 1 value can in input' = length(mb) == 1,
  #   'MB code not known' = mb %in% mb2016_to_sa12016$MB_CODE_2016)

  query_api(url = service_urls$urban_heat_island_value,
            where_query_string = sprintf("MB_CODE16='%s'", mb),
            returnGeometry = returnGeometry)

}
