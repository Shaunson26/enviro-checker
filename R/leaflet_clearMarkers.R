leaflet_clearMarkers <- function(mapId){
  leafletProxy(mapId = mapId) %>%
    clearMarkers() %>%
    nsw_bounds(flyThere = T)
}