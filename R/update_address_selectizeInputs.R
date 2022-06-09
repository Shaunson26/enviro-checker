update_address_selectizeInputs <- function(gnaf_subset, inputs, session){
  if (inputs$number == '') {
    unique_numbers <- unique_input_values(gnaf_subset$NUMBER_FIRST)
    updateSelectizeInputCustom(session = session,
                               inputId = 'number',
                               choices = unique_numbers)
    
  }
  
  if (inputs$street == '') {
    unique_streets <- unique_input_values(gnaf_subset$STREET_NAME)
    updateSelectizeInputCustom(session = session,
                               inputId = 'street',
                               choices = unique_streets)
  }
  
  if (inputs$suburb == '') {
    unique_suburbs <- unique_input_values(gnaf_subset$LOCALITY_NAME)
    updateSelectizeInputCustom(session = session,
                               inputId = 'suburb',
                               choices = unique_suburbs)
  }
  
  if (inputs$postcode == '') {
    unique_postcode <- unique_input_values(gnaf_subset$POSTCODE)
    updateSelectizeInputCustom(session = session,
                               inputId = 'postcode',
                               choices = unique_postcode)
  }
}