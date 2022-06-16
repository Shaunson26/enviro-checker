leaflet_clearMarkers <- function(mapId){
  leafletProxy(mapId = mapId) %>%
    clearMarkers() %>%
    clearGroup(group = 'boundaries') %>% 
    sydney_bounds(flyThere = F)
}