#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_new_data_set_server("new-data", reactive(input$section_selection))
  mod_head_server("head", reactive(input$section_selection))
  mod_foot_server("foot", reactive(input$section_selection))
  mod_body_server("body", reactive(input$section_selection))
}
