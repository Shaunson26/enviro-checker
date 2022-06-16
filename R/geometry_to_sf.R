#' Convert returned geometery field to SF
#' 
#' Do some wrangling of the \code{res$features[[1]]$geometry} field
#' 
#' @param res list, result from \code{get_*} function
geometry_to_sf <- function(res){
  
  if (length(res$features) != 0) {
    
    res$features[[1]]$geometry %>% 
      unlist() %>% 
      matrix(ncol = 2, byrow = T) %>% 
      as.data.frame() %>% 
      sf::st_as_sf(coords = c('V1', 'V2'), crs = 4326) %>% 
      sf::st_union() %>% 
      sf::st_cast('POLYGON') %>%
      sf::st_convex_hull() %>% 
      sf::st_as_sf()
  } else {
    NULL
  }
  
}
