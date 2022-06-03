# Selectsize functions
unique_input_values <- function(x) {
  c(NA, sort(unique(x)))
}

make_input <- function(id, label) {
  
  score_function <-
    "function scoreFilter(search) {

    this.getScoreFunction = function(query) { return this.sifter.getScoreFunction(query, $.extend(this.getSearchOptions(), {respect_word_boundaries: true}));	}

    var score = this.getScoreFunction(search);

    return function onScore(item) {
            var result = score(item);
            return result;
        }
    }}"
  
  score_function <-
    "function(search)
    {
      var score = this.getScoreFunction(search);
      return function(item)
      {
        return item.label
        .toLowerCase()
        .startsWith(search.toLowerCase()) ? 1 : 0;
      };
    }"
  
  selectizeInput(
    inputId = id,
    label = p(label),
    choices = NULL,
    options = list(placeholder = sprintf('type and select a %s', tolower(label)),
                   selectOnTab = TRUE,
                   score = base::I(score_function))
  )
}

updateSelectize <- function(session, inputId, choices) {
  updateSelectizeInput(
    session = session,
    inputId = inputId,
    choices = choices,
    server = TRUE,
    selected = ifelse(length(choices) == 2, choices[2], NA)
  )
}

reset_values <- function(session, list) {
  for (id in names(list)) {
    updateSelectizeInput(
      session = session,
      server = TRUE,
      inputId = id,
      choices = list[[id]]
    )
  }
}

# Filter
find_address <- function(data, number, street, suburb, postcode) {
  data %>%
    dplyr::filter(
      POSTCODE == as.integer(postcode),
      LOCALITY_NAME == suburb,
      STREET_NAME == street,
      NUMBER_FIRST == as.integer(number)
    )
}

# Map functions
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

blackMarker <-
  leaflet::icons(
    iconUrl = 'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png',
    shadowUrl = 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png',
    iconWidth= 25,
    iconHeight = 41,
    iconAnchorX = 12,
    iconAnchorY = 41,
    shadowAnchorX = 1,
    shadowAnchorY = -34,
    shadowWidth = 41,
    shadowHeight = 41
  )