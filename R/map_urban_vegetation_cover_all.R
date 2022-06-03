#' Wrangle urban vegetation cover results
#'
#' Wrangle list into  data.frame
#'
#' @param x, list, that from \code{map_urban_vegetation_cover_all}
#'
#' @return tibble / data.frame
#'
#' @export
map_urban_vegetation_cover_all <- function(x){

  attributes_labels <-
    c('PerAnyVeg' = 'Percent any vegetation',
      #"PerNonVeg" = 'Percent no vegetation',
      'PerGrass' = 'Percent grass',
      'PerShrub' = 'Percent shrubs',
      'PerTr03_10' = 'Percent short trees (3-10 m)',
      'PerTr10_15' = "Percent medium tress (10-15 m)",
      'PerTr15mPl' = "Percent tall trees (>15 m)")

  value_categories <-
    c('Less than 10%', '10 to 20%', '20 to 30%',
      '30 to 40%', 'More than 40%')

  categorise_value <- function(x) {
    cut(x, c(-1,10,20,30,40, Inf), labels = value_categories)
  }

  if (length(x$features) == 0){

    message('Address is outside the extent of data. No data is within the features element')
    values <- rep('no data', length(attributes_labels))
    value_text <- values

  } else {

    values <-
      sapply(x$features, function(x) {
        x$attributes[names(attributes_labels)] %>%
          as.numeric()
      }) %>%
      rowMeans()

    value_text = categorise_value(values)

  }

  tibble::tibble(label = unname(attributes_labels),
                 value_text = value_text,
                 value = values)

}
