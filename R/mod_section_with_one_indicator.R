#' section_with_one_indicator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_section_with_one_indicator_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' section_with_one_indicator Server Functions
#'
#' @noRd 
mod_section_with_one_indicator_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_section_with_one_indicator_ui("section_with_one_indicator_1")
    
## To be copied in the server
# mod_section_with_one_indicator_server("section_with_one_indicator_1")
