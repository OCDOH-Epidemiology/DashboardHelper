#' add_page_foot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_page_foot_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' add_page_foot Server Functions
#'
#' @noRd 
mod_add_page_foot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_add_page_foot_ui("add_page_foot_1")
    
## To be copied in the server
# mod_add_page_foot_server("add_page_foot_1")
