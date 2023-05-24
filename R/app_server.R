#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_new_data_set_server("new-data", reactive(input$section_selection))
  mod_page_head_server("page-head", reactive(input$section_selection))
  mod_page_foot_server("page-foot", reactive(input$section_selection))
  mod_section_with_one_indicator_server("one-indicator", reactive(input$section_selection))
  mod_section_with_multiple_indicators_server("multi-indicator", reactive(input$section_selection))
}
