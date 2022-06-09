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