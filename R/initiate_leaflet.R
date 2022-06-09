initiate_leaflet <- function(){
  leaflet(
    options = leafletOptions(dragging = FALSE)) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    nsw_bounds(flyThere = F)
}