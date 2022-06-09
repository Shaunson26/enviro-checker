leaflet_clearMarkers <- function(mapID ){
  leafletProxy(mapId = mapId) %>%
    clearMarkers() %>%
    nsw_bounds(flyThere = T)
}