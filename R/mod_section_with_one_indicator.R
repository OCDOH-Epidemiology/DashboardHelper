#' section_with_one_indicator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_section_with_one_indicator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("main"))
  )
}

#' section_with_one_indicator Server Functions
#'
#' @noRd
mod_section_with_one_indicator_server <- function(id, section_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$main <- shiny::renderUI({
      req(section_selection())

      if (section_selection() != "Section with one indicator") {
        return()
      }

      tagList(
        tags$h1("SINGLE INDICATOR"),
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
# mod_section_with_one_indicator_ui("one-indicator")

## To be copied in the server
# mod_section_with_one_indicator_server("one-indicator")
