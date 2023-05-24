#' new_data_set UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_new_data_set_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h1("New data set!")
  )
}
    
#' new_data_set Server Functions
#'
#' @noRd 
mod_new_data_set_server <- function(id, section_selection){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_new_data_set_ui("new-data")
    
## To be copied in the server
# mod_new_data_set_server("new-data")
