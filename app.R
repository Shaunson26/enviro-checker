library(shiny)
library(htmltools)
library(shinyjs)
library(magrittr)
library(shinybusy)
library(dplyr)
library(leaflet)
library(reactable)

for(x in list.files('R/', full.names = T)){ source(x); rm(x)}

gnaf <- readRDS('data/gnaf.rda')

gnaf_unique_column_list <-
  list(
    number = unique_input_values(gnaf$NUMBER_FIRST),
    street = unique_input_values(gnaf$STREET_NAME),
    suburb = unique_input_values(gnaf$LOCALITY_NAME),
    postcode = unique_input_values(gnaf$POSTCODE)
  )

# Widgets ----
address_selectizeInput_ui <-
  tagList(
    make_input(id = 'number', label = 'Street number'),
    make_input(id = 'street', label = 'Street name'),
    make_input(id = 'suburb', label = 'Suburb name'),
    make_input(id = 'postcode', label =  'Postcode')
  )

# UI ----
ui <- 
  navbarPage(
    title = NULL,
    windowTitle = 'Enviro-checker',
    id = 'main',
    header =
      tags$head(
        tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
        tags$link(rel="preconnect", href="https://fonts.gstatic.com", 'crossorigin'=NA),
        tags$link(href="https://fonts.googleapis.com/css2?family=Open+Sans&display=swap", rel="stylesheet"),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        useShinyjs()
      ),
    
    tabPanel(title = 'Select address', value = 'select-address',
             
             fluidRow(
               column(6, offset=3, id = 'select-address-text-container',
                      h1('Enviro-checker'),
                      p('Obtain general environmental characteristics of your NSW address.',
                        strong('NOTE: Data is really only available for Sydney metropolitan addresses.')),
                      p('Use the dropdown boxes to enter an address. Only street number is required, unit and apartment numbers are disregarded.')
               )
             ),
             
             tags$div(
               id = 'select-address-selectize-container',
               tags$div(
                 id = 'select-address-selectize-subcontainer',
                 address_selectizeInput_ui,
                 tags$div(id='select-address-button-container',
                          actionButton('search', 'Search', width = '70px'),
                          actionButton('clear', 'Clear', width = '70px')
                 )
               )
             )
    ),
    tabPanel(title = 'Results', value = 'results', id = 'results',
             
             tags$style(".ReactTable { background-color: rgba(255, 255, 255, 0.65);color: black}"),
             
             fluidRow(id = 'results-top',
                      column(width = 12, 
                             h2('Results'),
                             tags$div(id = 'results-address-container',
                                      tags$span('Input address: ', textOutput('address_text', container = strong)),
                                      actionButton('back', 'search again')
                             )
                             
                      )
             ),
             fluidRow(
               column(width = 12, 
                      h4('Map', a('what is this?', id='map-info', class='h4-link')),
                      leafletOutput("address_map", width = "100%", height = "300px")
               )
             ),
             
             fluidRow(id = 'back-to-top',
                      column(12,
                             br(),
                             tags$a('back to top', href='#results-top', style='padding: 8px 0; color: #FCF4D9;')
                      )
             )
             
    )
  )


# Server ----
server <- function(input, output, session) {
  
  # Initialize drop-downs
  reset_values(session = session, list = gnaf_unique_column_list)
  
  # Initialize map
  output$address_map <- renderLeaflet({
    # TODO Add boundaries
    leaflet(
      options = leafletOptions(dragging = FALSE)) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      nsw_bounds(flyThere = F)
  })
  
  outputOptions(output, "address_map", suspendWhenHidden = FALSE)
  
  # Listen for selections
  selection_listeners <- reactive({
    list(
      number = input$number,
      street = input$street,
      suburb = input$suburb,
      postcode = input$postcode
    )
  })
  
  # Check selections
  triggerSelectionUpdate = reactiveVal(Sys.time())
  
  observeEvent(selection_listeners(), ignoreInit = TRUE,
               handlerExpr =  {
                 
                 inputs_need_selection <- unlist(selection_listeners()) == ''
                 
                 if (any(inputs_need_selection)){
                   triggerSelectionUpdate(Sys.time())
                 }
                 
               })
  
  # Filtering GNAF
  triggerResults <- reactiveVal(Sys.time())
  
  gnaf_subset <-
    eventReactive(triggerSelectionUpdate(), ignoreInit = TRUE,
                  valueExpr =  {
                    
                    print('filtering GNAF')
                    
                    inputs = selection_listeners()
                    
                    gnaf %>%
                      filter(
                        if (inputs$number == '')
                          T
                        else
                          NUMBER_FIRST == as.integer(inputs$number),
                        if (inputs$street == '')
                          T
                        else
                          STREET_NAME == inputs$street ,
                        if (inputs$suburb == '')
                          T
                        else
                          LOCALITY_NAME == inputs$suburb ,
                        if (inputs$postcode == '')
                          T
                        else
                          POSTCODE == as.integer(inputs$postcode)
                      )
                    
                    
                  })
  
  # Update selections
  observeEvent(gnaf_subset(), {
    
    print('filtering GNAF done')
    
    inputs = selection_listeners()
    
    if (inputs$number == '') {
      unique_numbers <- unique_input_values(gnaf_subset()$NUMBER_FIRST)
      updateSelectize(session = session,
                      inputId = 'number',
                      choices = unique_numbers)
      
    }
    
    if (inputs$street == '') {
      unique_streets <- unique_input_values(gnaf_subset()$STREET_NAME)
      updateSelectize(session = session,
                      inputId = 'street',
                      choices = unique_streets)
    }
    
    if (inputs$suburb == '') {
      unique_suburbs <- unique_input_values(gnaf_subset()$LOCALITY_NAME)
      updateSelectize(session = session,
                      inputId = 'suburb',
                      choices = unique_suburbs)
    }
    
    if (inputs$postcode == '') {
      unique_postcode <- unique_input_values(gnaf_subset()$POSTCODE)
      updateSelectize(session = session,
                      inputId = 'postcode',
                      choices = unique_postcode)
    }
    
  })
  
  triggerDataPipeline = reactiveVal(Sys.time())
  
  observeEvent(input$search, {
    
    inputs <- unlist(selection_listeners())
    
    if (any(inputs == '')) {
      modalDialog(title = 'Error',
                  tags$p('Please complete all address fields'),
                  size = 's') %>%
        showModal()
    } else {
      triggerDataPipeline(Sys.time())
    }
  })
  
  observeEvent(triggerDataPipeline(),
               ignoreInit = TRUE,
               handlerExpr = {
                 
                 print('Finalising address')
                 
                 load('data/mb2016_to_sa12016.rda')

                 # input <-
                 #   list(number = 37,
                 #        street = 'ST PAULS CRESCENT',
                 #        suburb = 'LIVERPOOL',
                 #        postcode = 2170)
                 
                 found_address <-
                   #gnaf %>% 
                   gnaf_subset() %>%
                   find_address(number = input$number,
                                street = input$street,
                                suburb = input$suburb,
                                postcode = input$postcode) %>%
                   dplyr::mutate(LONGITUDE = LONGITUDE/10000,
                                 LATITUDE = LATITUDE/10000) %>% 
                   dplyr::left_join(mb2016_to_sa12016, by = c('MB_2016_CODE' = 'MB_CODE_2016'))
                 
                 print('Results')
                 
                 address_cols <- c('NUMBER_FIRST', 'STREET_NAME','LOCALITY_NAME', 'POSTCODE')
                 address_text <- paste(unlist(found_address[, address_cols]), collapse = ' ')
                 
                 output$address_text <- renderText({ address_text })
                 
                 shinybusy::show_modal_spinner(text = 'Obtaining data ...')
                 
                 print('Getting UHI')
                 
                 uhi_resp <-
                   found_address %>%
                   dplyr::pull(MB_2016_CODE) %>%
                   get_urban_heat_island_value() %>%
                   map_urban_heat_island_value() %>% 
                   add_uhi_css()
                 
                 print('Getting UVI')
                 
                 hvi_resp <-
                   found_address %>%
                   pull(SA1_MAINCODE_2016) %>%
                   get_heat_vulnerability_index() %>%
                   map_heat_vulnerability_index() %>% 
                   add_hvi_css()
                 
                 print('Getting UVCA')
                 
                 uvca_resp <-
                   found_address %>%
                   dplyr::pull(MB_2016_CODE) %>%
                   get_urban_vegetation_cover_all() %>%
                   map_urban_vegetation_cover_all() %>% 
                   add_uvca_css()
                 
                 # Insert UIs
                 # UHI ----
                 insertUI(selector = '#back-to-top', where = 'beforeBegin',
                          ui = 
                            fluidRow(id = 'uhi-container',
                                     column(width = 12,
                                            h4('Urban Heat Island',
                                               a('what is this?', id='uhi-info', class='h4-link')
                                            ),
                                            htmlOutput("uhi_results_mobile", class='mobile-result-container'),
                                            reactableOutput('uhi_results_desktop')
                                     )
                            )
                 )
                 
                 output$uhi_results_mobile <- renderUI({
                   div(class='mobile-result-item',
                       p(uhi_resp$label),
                       create_horizontal_bar_div(uhi_resp, 
                                                 minText = '', maxText = ''),
                       hr(style='opacity:0.5;margin: 8px 0;')
                   )
                 })
                 
                 output$uhi_results_desktop <- renderReactable({ 
                   uhi_resp %>% 
                     uhi_reactable() 
                 })
                 
                 outputOptions(output, "uhi_results_mobile", suspendWhenHidden = FALSE)
                 outputOptions(output, "uhi_results_desktop", suspendWhenHidden = FALSE)
                 
                 # HVI ----
                 insertUI(selector = '#back-to-top', where = 'beforeBegin',
                          ui = fluidRow(id = 'hvi-container',
                                        column(width = 12,
                                               h4('Heat vulnerability index',
                                                  a('what is this?', id='hvi-info', class='h4-link')),
                                               htmlOutput("hvi_results_mobile", class='mobile-result-container'),
                                               reactableOutput('hvi_results_desktop')
                                        )
                          )
                 )
                 
                 output$hvi_results_mobile <- renderUI({
                   
                   lapply(1:nrow(hvi_resp), function(i){
                     div(class='mobile-result-item',
                         p(hvi_resp$label[i]),
                         create_horizontal_bar_div(hvi_resp[i,]),
                         hr(style='opacity:0.5;margin: 8px 0;')
                     )
                   }) %>% 
                     tagList() 
                 })
                 
                 output$hvi_results_desktop <- renderReactable({
                   hvi_resp %>% 
                     hvi_reactable()
                 })
                 
                 outputOptions(output, "hvi_results_mobile", suspendWhenHidden = FALSE)
                 outputOptions(output, "hvi_results_desktop", suspendWhenHidden = FALSE)
                 
                 # UVCA ----
                 insertUI(selector = '#back-to-top', where = 'beforeBegin',
                          ui = fluidRow(id = 'uvca-container',
                                        column(12,
                                               h4('Urban vegetation cover',
                                                  a('what is this?', id='uvca-info', class='h4-link')),
                                               htmlOutput("uvca_results_mobile", class='mobile-result-container'),
                                               reactableOutput('uvca_results_desktop')
                                        )
                          )
                 )
                 
                 output$uvca_results_mobile <- renderUI({
                   lapply(1:nrow(uvca_resp), function(i){
                     div(class='mobile-result-item',
                         p(uvca_resp$label[i]),
                         create_horizontal_bar_div(uvca_resp[i,]),
                         hr(style='opacity:0.5;margin: 8px 0;')
                     )
                   }) %>% 
                     tagList() %>% browsable()
                 })
                 
                 output$uvca_results_desktop <- renderReactable({
                   uvca_resp %>% 
                     uvca_reactable()
                 })
                 
                 outputOptions(output, "uvca_results_mobile", suspendWhenHidden = FALSE)
                 outputOptions(output, "uvca_results_desktop", suspendWhenHidden = FALSE)
                 
                 shinybusy::remove_modal_spinner()
                 
                 updateNavbarPage(inputId = 'main', selected = 'results')
                 
                 leafletProxy("address_map") %>%
                   clearMarkers() %>%
                   addMarkers(
                     lng = found_address$LONGITUDE,
                     lat = found_address$LATITUDE,
                     label = address_text,
                     icon = blackMarker
                   ) %>%
                   flyTo(lng =  found_address$LONGITUDE,
                         lat = found_address$LATITUDE,
                         zoom = 14)
               })
  
  observeEvent(input$clear, {
    reset_values(session = session, list = gnaf_unique_column_list)
  })
  
  observeEvent(input$back, {
    reset_values(session = session, list = gnaf_unique_column_list)
    
    updateNavbarPage(inputId = 'main', selected = 'select-address')
    leafletProxy("address_map") %>%
      clearMarkers() %>%
      nsw_bounds(flyThere = T)
    
    #removeUI('#found_address')
    removeUI('#uhi-container')
    removeUI('#hvi-container')
    removeUI('#uvca-container')
  })
  
  shinyjs::onclick('map-info', {
    modalDialog(title = 'Map panel',
                p('This panel shows the approximate location of the address.')) %>% 
      showModal()
  })
  
  shinyjs::onclick('uhi-info', {
    modalDialog(title = 'The Urban Heat Island (UHI) panel',
                p(uhi_text$text),
                a(uhi_text$link)) %>% 
      showModal()
  })
  
  shinyjs::onclick('hvi-info', {
    modalDialog(title = 'Heat Vulnerability Index (HVI)',
                p(hvi_text$text),
                a(hvi_text$link)) %>% 
      showModal()
  })
  
  shinyjs::onclick('uvca-info', {
    modalDialog(title = 'NSW Urban Vegetation Cover panel',
                p(uvca_text$text),
                a(uvca_text$link)) %>% 
      showModal()
  })
  
}
# Run app ----
shinyApp(ui = ui, server = server)
