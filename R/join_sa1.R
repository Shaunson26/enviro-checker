#' Join Statistical Area 1 2016 to Mesh Block 2016
#'
#' MB 2016 is present in the GANF, we attach SA1 using a concordance dataset \code{mb2016_to_sa12016}
#'
#' @param x data.frame, data with MB_2016_CODE
#'
#' @export
join_sa1 <- function(x){
  dplyr::left_join(x, mb2016_to_sa12016, by = c('MB_2016_CODE' = 'MB_CODE_2016'))
}
