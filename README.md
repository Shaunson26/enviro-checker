# Enviro-checker

A simple R Shiny app that takes address information, queries environmental data
APIs, and displays results.  

https://shaun-nielsen.shinyapps.io/Enviro-checker/

## Address data

Address data is provided by Geoscape G-NAF and the Australian Government:

Geoscape G-NAF is Australia’s authoritative, geocoded address file. It is built
and maintained by Geoscape Australia using authoritative government data.
G-NAF is one of the most ubiquitous and powerful spatial datasets. It contains
more than 14 million Australian physical address records. The records include
geocodes, which are latitude and longitude map coordinates. G-NAF does not
contain personal names.

This dataset was originally found at:
https://data.gov.au/data/dataset/geocoded-national-address-file-g-naf

NSW addresses were extracted, and further simplified to contain only street address,
along with lat/lon and meshb;ock code. See `R/gnaf.R` and `data/gnaf.rda`

## Shiny App

- Uses a `navbarPage()` with search fields in the first tab and results in the second tab.
The navbar is hidden to give the experience of two different pages

- The address select boxes use server side `selectizeInput()` 
    - update when any one of the boxes have user input (and can auto-complete if the update results in a single address)
    - match using the start of the word rather than the default of any sub-string
    - throw an error modal if the search button is clicked without full address information
    
- The address submitted has associated lat/lon and geographic boundaries. These are used to query
NSW government environmental data APIs using `httr2`. A few heat related ones were chosen (may update in future).

- `shinybusy` is used for data download modals, which take a few seconds.


- The returned JSON data is wrangled using `dplyr` and then displayed
    - A `leaflet` map is used to show the location of the address using the lat/lon data
    - Environmental data obtain is displayed with `reactable` and simple bar plots (using HTML elements from `htmltools`)
    
- The results page is responsive and displays
   - a set of reactable results for wide screen
   - a set of result divs for mobile
   
- The results sections have a 'what is this?' link to describe the environmental 
parameters (shamelessly cut and paste from the source). This is achieved using `shinyjs`.



