insert_result_ui <- function(title, id_container, id_info, ...){
  
  ui <-
    fluidRow(id = id_container,
             column(
               width = 12,
               h4(title,
                  a('what is this?', id = id_info, class = 'h4-link')
               ),
               ...
               # htmlOutput("uhi_results_mobile", class = 'mobile-result-container'),
               # reactableOutput('uhi_results_desktop')
             ))
  
  ui
  
  insertUI(
    selector = '#back-to-top',
    where = 'beforeBegin',
    ui = ui)
  
}
