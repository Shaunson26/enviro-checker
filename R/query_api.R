#' Query ArcGIS REST Service using httr2
#'
#' Hopefully they're all the same! Heat Vulnerability Index
#'
#' @param url string, API URL
#' @param where_query_string string, where clause e.g. "SA1_MAIN16=123456"
#'
#' @export
query_api <- function(url, where_query_string, returnGeometry = FALSE){
  
  returnGeometry = ifelse(returnGeometry, 'true', 'false')

  resp <-
    httr2::request(url) %>%
    httr2::req_url_query(f='pjson',
                         returnGeometry=returnGeometry,
                         outFields="*",
                         where=where_query_string) %>%
    httr2::req_perform()

  if (httr2::resp_status(resp) == 200){

    resp_list <-
      resp %>%
      httr2::resp_body_json(check_type = FALSE)

    if (length(resp_list$features) == 0){
      warning('Address is outside the extent of data. No data is within the features element')
    }

    return(resp_list)

  } else {
    stop(httr2::resp_status_desc(resp))
  }

}
