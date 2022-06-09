info_onclick_modal <- function(x){
  modalDialog(title = x$title,
              p(x$text, style='text-align: justify;'),
              a(x$link)) %>%
    showModal()
}
