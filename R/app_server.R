#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_page_head_server("page-head", reactive(input$section_selection))
  mod_page_foot_server("page-foot", reactive(input$section_selection))
}
