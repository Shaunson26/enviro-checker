leaflet_flyTo_address <-
  function(mapId,
           found_address,
           boundaries,
           label,
           zoom = 14) {
    leafletProxy(mapId) %>%
      clearMarkers() %>%
      addMarkers(
        lng = found_address$LONGITUDE,
        lat = found_address$LATITUDE,
        label = label,
        icon = blackMarker
      )
    
    if (nrow(boundaries) > 0) {
      print('using boundaries')
      bbox <- unname(sf::st_bbox(boundaries))
      
      leafletProxy(mapId) %>%
        addPolygons(
          data = boundaries,
          label = c('Data boundary: HVI', 'Data boundary: UHI & UVC'),
          color = 'black',
          group = 'boundaries'
        ) %>%
        flyToBounds(#fitBounds
          lng1 = bbox[1],
          lat1 = bbox[2],
          lng2 = bbox[3],
          lat2 = bbox[4]
        )
    } else {
      print('flying to points')
      leafletProxy(mapId) %>%
        flyTo(lng =  found_address$LONGITUDE,
              lat = found_address$LATITUDE,
              zoom = zoom)
    }
    
  }
