nsw_bounds <- function(map, flyThere = F) {
  # nsw_bbox = c(minX = 140, minY = -40, maxX = 155, maxY = -27)
  if (flyThere) {
    flyToBounds(
      map = map,
      lng1 = 140,
      lng2 = 155,
      lat1 = -40,
      lat2 = -27
    )
  } else {
    fitBounds(
      map = map,
      lng1 = 140,
      lng2 = 155,
      lat1 = -40,
      lat2 = -27
    )
  }
}