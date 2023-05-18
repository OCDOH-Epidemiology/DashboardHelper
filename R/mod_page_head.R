#' page_head UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_head_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("page_head"))
  )
}

#' page_head Server Functions
#'
#' @noRd
mod_page_head_server <- function(id, section_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$page_head <- shiny::renderUI({
      req(section_selection())

      if (section_selection() != "Page Header") {
        return()
      }

      tagList(
        shiny::textInput(ns("main_indicator"), tags$h3("Indicator", class = "m-0")),
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            shiny::sliderInput(ns("num_paragraphs"), "How many paragraphs should be in the header?", 1, 10, 1, 1, width = "100%"),
            shiny::uiOutput(ns("paragraph_inputs"))
          ),
          tags$div(
            class = "col-md-6",
            shiny::sliderInput(ns("num_buttons"), "How many section buttons are there?", 1, 10, 1, 1, width = "100%"),
            shiny::uiOutput(ns("button_inputs"))
          )
        ),
        shiny::uiOutput(ns("preview")),
        tags$br(),
        shiny::verbatimTextOutput(ns("generated_code"))
      )
    })

    output$paragraph_inputs <- shiny::renderUI({
      req(input$num_paragraphs)

      lapply(1:input$num_paragraphs, function(paragraph_number) {
        shiny::textInput(ns(paste0("head_paragraph_", paragraph_number)), paste0("Paragraph ", paragraph_number, " Text"), width = "100%")
      })
    })

    output$button_inputs <- shiny::renderUI({
      req(input$num_buttons)

      lapply(1:input$num_buttons, function(button_number) {
        shiny::textInput(ns(paste0("head_button_", button_number)), paste0("Button ", button_number, " Text"), width = "100%")
      })
    })

    output$preview <- shiny::renderUI({
      tagList(
        tags$h2("Preview"),
        add_page_head(
          indicator = input$main_indicator,
          description = lapply(1:input$num_paragraphs, function(paragraph_number) {
            input[[paste0("head_paragraph_", paragraph_number)]]
          }),
          sections = lapply(1:input$num_buttons, function(button_number) {
            input[[paste0("head_button_", button_number)]]
          })
        )
      )
    })

    output$generated_code <- shiny::renderPrint({
      paragraphs_combined <- ""

      for (paragraph_number in 1:input$num_paragraphs) {
        paragraphs_combined <- paste0(paragraphs_combined, "    \"", input[[paste0("head_paragraph_", paragraph_number)]], "\"")

        if (paragraph_number < max(input$num_paragraphs)) {
          paragraphs_combined <- paste0(paragraphs_combined, ",\n")
        }
      }

      buttons_combined <- ""

      for (button_number in 1:input$num_buttons) {
        buttons_combined <- paste0(buttons_combined, "    \"", input[[paste0("head_button_", button_number)]], "\"")

        if (button_number < max(input$num_buttons)) {
          buttons_combined <- paste0(buttons_combined, ",\n")
        }
      }

      cat(
        "add_page_head(",
        paste0("  indicator = \"", input$main_indicator, "\","),
        "  description = list(",
        paragraphs_combined,
        "  ),",
        "  sections = list(",
        buttons_combined,
        "  )",
        "),",
        sep = "\n"
      )
    })
  })
}

## To be copied in the UI
# mod_page_head_ui("page-head")

## To be copied in the server
# mod_page_head_server("page-head")
