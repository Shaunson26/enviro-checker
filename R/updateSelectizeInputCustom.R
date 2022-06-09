updateSelectizeInputCustom <- function(session, inputId, choices) {
  updateSelectizeInput(
    session = session,
    inputId = inputId,
    choices = choices,
    server = TRUE,
    selected = ifelse(length(choices) == 2, choices[2], NA)
  )
}