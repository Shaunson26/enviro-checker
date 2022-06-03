library(shiny)

select_address_ui <- function(id='select-address'){
  
  ns <- NS(id)
  
  tags$div(
    selectizeInput(ns('this1'), label = 'Street number', choices = c('', 1:10), ),
    actionButton(ns('go'), 'Search', width = '70px'),
  )
}

select_address_server <- function(id='select-address'){
  
  moduleServer(id, 
               function(input, output, session) {
                 
                 eventReactive(input$go, 
                               
                               if(input$this1 == '') {
                                 showModal(modalDialog('Ded'))
                                 return(NULL)
                               } else {
                                 return(input$this1)
                               })
                 
               })
}

select_address_demo <- function(){
  
  ui <- fluidPage(
    select_address_ui()
  )
  
  server <- 
    function(input, output, session) {
      
      # select_address_server returns a reactive object
      a_trigger <- select_address_server()
      
      observeEvent(a_trigger(), ignoreNULL = TRUE, 
                   handlerExpr = {
                     print(a_trigger())
                   })
      
    }
  
  shinyApp(ui, server)
}

select_address_demo()
