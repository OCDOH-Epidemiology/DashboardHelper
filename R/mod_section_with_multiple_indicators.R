#' section_with_multiple_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_section_with_multiple_indicators_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("main"))
  )
}

#' section_with_multiple_indicators Server Functions
#'
#' @noRd
mod_section_with_multiple_indicators_server <- function(id, section_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$main <- shiny::renderUI({
      req(section_selection())

      if (section_selection() != "Section with multiple indicators") {
        return()
      }

      tagList(
        tags$h1("MULTI INDICATOR"),
        shiny::uiOutput(ns("preview")),
        tags$br(),
        shiny::verbatimTextOutput(ns("generated_code"))
      )
    })

    output$preview <- shiny::renderUI({
      "Hello World!"
    })

    output$generated_code <- renderPrint({
      "Hello World!"
    })
  })
}

## To be copied in the UI
# mod_section_with_multiple_indicators_ui("multi-indicator")

## To be copied in the server
# mod_section_with_multiple_indicators_server("multi-indicator")
