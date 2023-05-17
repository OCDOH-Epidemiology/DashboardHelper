#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_page_head_server("page-head", reactive(input$section_selection))
  mod_add_section_with_one_indicator_server("single-indicator")
  mod_add_section_with_multiple_indicators_server("multiple-indicators")
  mod_add_page_foot_server("page-foot")
}
