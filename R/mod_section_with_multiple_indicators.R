#' section_with_multiple_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_section_with_multiple_indicators_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' section_with_multiple_indicators Server Functions
#'
#' @noRd 
mod_section_with_multiple_indicators_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_section_with_multiple_indicators_ui("section_with_multiple_indicators_1")
    
## To be copied in the server
# mod_section_with_multiple_indicators_server("section_with_multiple_indicators_1")
