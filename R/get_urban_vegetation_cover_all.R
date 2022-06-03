#' NSW Heat Vulnerability Index to ABS Statistical Area Level 1 2016
#'
#' The NSW Urban Vegetation Cover to Modified Mesh Block 2016 provides both an
#' area and percentage of vegetation for city blocks and infrastructure corridors
#' in the Sydney Greater Metropolitan Area as of 2016. With this dataset, users can
#' estimate tree canopy and vegetation cover in urban areas at many scales, such as
#' mesh block, precinct, or local government area. Having current and accurate estimates
#' of tree canopy and vegetation like this supports citizens and governments to reliably
#' identify areas of tree canopy and confidently develop urban greening and heat
#' island mitigation strategies and action. This dataset provides the user with
#' information of high spatial accuracy. The dataset uses vegetation information
#' derived from high resolution aerial photography combined with boundary and
#' land use information from the Australian Bureau of Statistics (ABS) Mesh Block
#' polygon dataset augmented with road and railroad data from the NSW Digital
#' Cadastral Database. The content was co-designed with state and local governments
#' and developed using scientifically-rigorous methodologies. The extent of the
#' dataset covers urban, major urban, peri-urban and other urban areas within the
#' Sydney Greater Metropolitan. While the dataset provides wall to wall coverage
#' of many councils, it does not include far outlying rural areas in local government
#' areas with a largely rural component.
#'
#' https://datasets.seed.nsw.gov.au/dataset/nsw-urban-vegetation-cover-to-modified-mesh-block-2016
#'
#' @param mb text, Mesh block 2016 code
#'
#' @export
get_urban_vegetation_cover_all <- function(mb){

  # stopifnot(
  #   'Only 1 value can in input' = length(mb) == 1,
  #   'MB code not known' = mb %in% mb2016_to_sa12016$MB_CODE_2016)

  query_api(url = service_urls$urban_vegetation_cover_all,
            where_query_string = sprintf("MB_CODE16='%s'", mb))

}
