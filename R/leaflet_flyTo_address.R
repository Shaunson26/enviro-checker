leaflet_flyTo_address <- function(mapId, found_address, label, zoom = 14){
  leafletProxy(mapId) %>%
    clearMarkers() %>%
    addMarkers(
      lng = found_address$LONGITUDE,
      lat = found_address$LATITUDE,
      label = label,
      icon = blackMarker
    ) %>%
    flyTo(lng =  found_address$LONGITUDE,
          lat = found_address$LATITUDE,
          zoom = zoom)
}
