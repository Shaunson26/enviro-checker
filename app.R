library(shiny)
library(htmltools)
library(shinyjs)
library(magrittr)
library(shinybusy)
library(dplyr)
library(leaflet)
library(reactable)
library(promises)
library(future)

plan(multisession)

for (x in list.files('R/', full.names = T)) {
  source(x)
  rm(x)
}

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
    selectizeInputCustom(id = 'number', label = 'Street number'),
    selectizeInputCustom(id = 'street', label = 'Street name'),
    selectizeInputCustom(id = 'suburb', label = 'Suburb name'),
    selectizeInputCustom(id = 'postcode', label =  'Postcode')
  )

# UI ----
ui <-
  navbarPage(
    title = NULL,
    windowTitle = 'Enviro-checker',
    id = 'main',
    header =
      tags$head(
        tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        tags$link(
          rel = "preconnect",
          href = "https://fonts.gstatic.com",
          'crossorigin' = NA
        ),
        tags$link(href = "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap", rel =
                    "stylesheet"),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        useShinyjs()
      ),
    
    tabPanel(
      title = 'Select address',
      value = 'select-address',
      
      fluidRow(
        column(
          6,
          offset = 3,
          id = 'select-address-text-container',
          h1('Heati-Greeni-Info'),
          p('Obtain heat and vegetation information of your greater Sydney address using data from the Department of Planning and Environment'),
          p('Use the dropdown boxes to enter an address. Only street number is required, unit and apartment numbers are disregarded.',
            style = "font-size: 12px;"
          )
        )
      ),
      
      tags$div(
        id = 'select-address-selectize-container',
        tags$div(
          id = 'select-address-selectize-subcontainer',
          address_selectizeInput_ui,
          tags$div(
            id = 'select-address-button-container',
            actionButton('search', 'Search', width = '70px'),
            actionButton('clear', 'Clear', width = '70px')
          )
        )
      ),
      fluidRow(
        column(12,
               p('About this app', id='about-app', 
                 style = "font-size: 12px;text-align: center; padding-top: 16px;cursor:help;")
        )
      )
    ),
    tabPanel(
      title = 'Results',
      value = 'results',
      id = 'results',
      
      tags$style(
        ".ReactTable { background-color: rgba(255, 255, 255, 0.65);color: black}"
      ),
      
      fluidRow(id = 'results-top',
               column(
                 width = 12,
                 h2('Results'),
                 tags$div(
                   id = 'results-address-container',
                   tags$span(
                     'Input address: ',
                     textOutput('address_text', container = strong)
                   ),
                   actionButton('back', 'search again')
                 )
                 
               )),
      fluidRow(column(
        width = 12,
        h4('Map', a(
          'what is this?', id = 'map-info', class = 'h4-link'
        )),
        leafletOutput("address_map", width = "100%", height = "300px")
      )),
      
      fluidRow(id = 'back-to-top',
               column(
                 12,
                 br(),
                 tags$a('back to top', href = '#results-top', style =
                          'padding: 8px 0; color: #FCF4D9;')
               ))
      
    )
  )


# Server ----
server <- function(input, output, session) {
  
  shinyjs::onclick('about-app', { info_onclick_modal(app_text) })
  
  # Initialize drop-downs
  reset_values(session = session, list = gnaf_unique_column_list)
  
  # Initialize map
  output$address_map <- renderLeaflet({
    # TODO Add boundaries
    initiate_leaflet()
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
  
  observeEvent(selection_listeners(),
               ignoreInit = TRUE,
               handlerExpr =  {
                 # TODO updating after selection
                 inputs_need_selection <-
                   unlist(selection_listeners()) == ''
                 
                 if (any(inputs_need_selection)) {
                   triggerSelectionUpdate(Sys.time())
                 }
                 
               })
  
  # Filtering GNAF
  triggerResults <- reactiveVal(Sys.time())
  
  gnaf_subset <-
    eventReactive(triggerSelectionUpdate(),
                  ignoreInit = TRUE,
                  valueExpr =  {
                    gnaf %>%
                      filter_gnaf(inputs = selection_listeners())
                    
                  })
  
  # Update selections
  observeEvent(gnaf_subset(), {
    gnaf_subset() %>%
      update_address_selectizeInputs(inputs = selection_listeners(), session = session)
    
  })
  
  # Check inputs
  triggerDataPipeline = reactiveVal(Sys.time())
  
  observeEvent(input$search, {
    
    inputs <- unlist(selection_listeners())
    
    if (any(inputs == '')) {
      showModal(please_compete_address_fields_modal())
    } else {
      triggerDataPipeline(Sys.time())
    }
  })
  
  observeEvent(triggerDataPipeline(),
               ignoreInit = TRUE,
               handlerExpr = {
                 print('Finalising address')
                 
                 load('data/mb2016_to_sa12016.rda')
                 
                 found_address <-
                   gnaf_subset() %>%
                   filter_gnaf(inputs = selection_listeners(), test = FALSE) %>% 
                   dplyr::mutate(LONGITUDE = LONGITUDE / 10000,
                                 LATITUDE = LATITUDE / 10000) %>%
                   dplyr::left_join(mb2016_to_sa12016, by = c('MB_2016_CODE' = 'MB_CODE_2016'))
                 
                 print('Results')
                 
                 address_text <- create_address_text(found_address)
                 
                 output$address_text <- renderText({
                   address_text
                 })
                 
                 shinybusy::show_modal_spinner(text = 'Obtaining data ...')
                 
                 print('Getting UHI')
                 
                 uhi_resp <-
                   future({
                     found_address %>%
                       dplyr::pull(MB_2016_CODE) %>%
                       get_urban_heat_island_value() %>%
                       map_urban_heat_island_value() %>%
                       add_uhi_css()
                   })
                 
                 print('Getting UVI')
                 
                 hvi_resp <-
                   future({
                     found_address %>%
                       pull(SA1_MAINCODE_2016) %>%
                       get_heat_vulnerability_index() %>%
                       map_heat_vulnerability_index() %>%
                       add_hvi_css()
                   })
                 
                 print('Getting UVCA')
                 
                 uvca_resp <-
                   future({
                     found_address %>%
                       dplyr::pull(MB_2016_CODE) %>%
                       get_urban_vegetation_cover_all() %>%
                       map_urban_vegetation_cover_all() %>%
                       add_uvca_css()
                   })
                 
                 api_data <-
                   reactive({
                     list(
                       hvi_resp = value(hvi_resp),
                       uhi_resp = value(uhi_resp),
                       uvca_resp = value(uvca_resp)
                     )
                   })
                 
                 observeEvent(api_data(), {
                   print('data downloaded!')
                   shinybusy::remove_modal_spinner()
                   updateNavbarPage(inputId = 'main', selected = 'results')
                   leaflet_flyTo_address(mapId = 'address_map', 
                                         label = address_text,
                                         found_address = found_address)
                 })
                 
                 # Insert UIs
                 # UHI ----
                 insert_result_ui(title = 'Urban heat island', 
                                  id_container = 'uhi-container', id_info = 'uhi-info', 
                                  htmlOutput("uhi_results_mobile", class = 'mobile-result-container'),
                                  reactableOutput('uhi_results_desktop'))
                 
                 output$uhi_results_mobile <- renderUI({
                   uhi_resp %...>% 
                     create_mobile_results()
                 })
                 
                 output$uhi_results_desktop <- renderReactable({
                   uhi_resp %...>%
                     uhi_reactable()
                 })
                 
                 outputOptions(output, "uhi_results_mobile", suspendWhenHidden = FALSE)
                 outputOptions(output, "uhi_results_desktop", suspendWhenHidden = FALSE)
                 
                 # HVI ----
                 insert_result_ui(title = 'Heat vulnerability index', 
                                  id_container = 'hiv-container', id_info = 'hvi-info', 
                                  htmlOutput("hvi_results_mobile", class = 'mobile-result-container'),
                                  reactableOutput('hvi_results_desktop'))
                 
                 output$hvi_results_mobile <- renderUI({
                   hvi_resp %...>% 
                     create_mobile_results()
                 })
                 
                 output$hvi_results_desktop <- renderReactable({
                   hvi_resp %...>%
                     hvi_reactable()
                 })
                 
                 outputOptions(output, "hvi_results_mobile", suspendWhenHidden = FALSE)
                 outputOptions(output, "hvi_results_desktop", suspendWhenHidden = FALSE)
                 
                 # UVCA ----
                 insert_result_ui(title = 'Urban vegetation cover', 
                                  id_container = 'uvca-container', id_info = 'uvca-info', 
                                  htmlOutput("uvca_results_mobile", class = 'mobile-result-container'),
                                  reactableOutput('uvca_results_desktop'))
                 
                 
                 
                 output$uvca_results_mobile <- renderUI({
                   uvca_resp %...>% 
                     create_mobile_results()
                 })
                 
                 output$uvca_results_desktop <- renderReactable({
                   uvca_resp %...>%
                     uvca_reactable()
                 })
                 
                 outputOptions(output, "uvca_results_mobile", suspendWhenHidden = FALSE)
                 outputOptions(output, "uvca_results_desktop", suspendWhenHidden = FALSE)
                 
               })
  
  
  
  
  
  
  
  observeEvent(input$clear, {
    reset_values(session = session, list = gnaf_unique_column_list)
  })
  
  observeEvent(input$back, {
    reset_values(session = session, list = gnaf_unique_column_list)
    
    updateNavbarPage(inputId = 'main', selected = 'select-address')
    
    leaflet_clearMarkers(mapId = "address_map")
    
    removeUI('#uhi-container')
    removeUI('#hvi-container')
    removeUI('#uvca-container')
  })
  
  shinyjs::onclick('map-info', { info_onclick_modal(map_text) })
  shinyjs::onclick('uhi-info', { info_onclick_modal(uhi_text) })
  shinyjs::onclick('hvi-info', { info_onclick_modal(hvi_text)  })
  shinyjs::onclick('uvca-info', { info_onclick_modal(uvca_text) })
  
}
# Run app ----
shinyApp(ui = ui, server = server)
