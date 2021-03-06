app_text <-
  list(title = 'About this app',
       text = 'This is an R Shiny application. It binds together the R Statistical Programming language with Web Development to create web applications. I created this app as a project explore how Shiny works while also playing around with data integration and using data APIs. My github repo explains more:',
       link = 'https://github.com/Shaunson26/enviro-checker')

map_text <- 
  list(title='Map panel',
       text = "This panel shows the approximate location of the address.",
       link = NULL)

uhi_text <- 
  list(title='Urban Heat Island (UHI) panel',
       text = "The Urban Heat Island (UHI) dataset measures the effects of urbanisation on land surface temperatures across Sydney Greater Metropolitan Area for the Summer of 2015-2016.
UHI shows the variation of temperature to a non-urban vegetated reference, such as heavily wooded areas or national parks around Sydney.
Derived from the analysis of thermal and infrared data from Landsat satellite, the dataset has been combined with the Australian Bureau of Statistics (ABS) Mesh Block polygon dataset to provide a mean UHI temperature that enables multi-scale spatial analysis of the relationship of heat to green cover.",
       link = 'https://datasets.seed.nsw.gov.au/dataset/nsw-urban-heat-island-to-modified-mesh-block-2016')

hvi_text <-
  list(title= 'Heat Vulnerability Index (HVI)',
       text = "The NSW Heat Vulnerability Index (HVI) dataset identifies areas to monitor where populations in the Sydney Greater Metropolitan Area are more vulnerable to the adverse effects of urban heat, as of Summer 2015-2016. HVI utilises indicators for exposure, sensitivity and adaptive capacity to calculate an overall heat vulnerability index. Expressed through the data, a vulnerability of 1 represents a combination of low exposure, low sensitivity and/or high adaptive capacity. A vulnerability of 5 represents high exposure, high sensitivity and/or low adaptive capacity. The calculation of HVI and the inputs to the exposure, sensitivity and adaptive capacity indicators are explained in the metadata. The HVI data is aggregated to the Australian Bureau of Statistics (ABS) Statistical Area Level 1 (SA1) polygon dataset to enable spatial analysis to support local policy and decision making. It can be used in conjunction with the NSW urban vegetation cover dataset for the same time period for broader analysis of the relationship of heat to green cover.",
       link = "https://datasets.seed.nsw.gov.au/dataset/nsw-heat-vulnerability-index-to-abs-statistical-area-level-1-2016")

uvca_text <-
  list(title= 'Urban Vegetation Cover panel',
       text = "The NSW Urban Vegetation Cover to Modified Mesh Block 2016 provides both an area and percentage of vegetation for city blocks and infrastructure corridors in the Sydney Greater Metropolitan Area as of 2016. With this dataset, users can estimate tree canopy and vegetation cover in urban areas at many scales, such as mesh block, precinct, or local government area. Having current and accurate estimates of tree canopy and vegetation like this supports citizens and governments to reliably identify areas of tree canopy and confidently develop urban greening and heat island mitigation strategies and action.
This dataset provides the user with information of high spatial accuracy. The dataset uses vegetation information derived from high resolution aerial photography combined with boundary and land use information from the Australian Bureau of Statistics (ABS) Mesh Block polygon dataset augmented with road and railroad data from the NSW Digital Cadastral Database. The content was co-designed with state and local governments and developed using scientifically-rigorous methodologies. The extent of the dataset covers urban, major urban, peri-urban and other urban areas within the Sydney Greater Metropolitan. While the dataset provides wall to wall coverage of many councils, it does not include far outlying rural areas in local government areas with a largely rural component.",
       link = "https://datasets.seed.nsw.gov.au/dataset/nsw-urban-vegetation-cover-to-modified-mesh-block-2016")
