#' page_foot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_foot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("page_foot"))
  )
}

#' add_page_foot Server Functions
#'
#' @noRd
mod_page_foot_server <- function(id, section_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$page_foot <- shiny::renderUI({
      req(section_selection())

      if (section_selection() != "Page Footer") {
        return()
      }

      tagList(
        shiny::sliderInput(ns("num_footnotes"), "How many footnotes are there?", 1, 15, 1, 1, width = "50%"),
        tags$h3("If the footnote is not a link, leave the URL blank."),
        shiny::uiOutput(ns("footnote_inputs")),
        shiny::uiOutput(ns("result")),
        tags$br(),
        shiny::verbatimTextOutput(ns("generated_code"))
      )
    })

    output$footnote_inputs <- shiny::renderUI({
      req(input$num_footnotes)

      lapply(1:input$num_footnotes, function(footnote_number) {
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            shiny::textInput(ns(paste0("footnote_text_", footnote_number)), paste0("Footnote ", footnote_number, " Text"), width = "100%")
          ),
          tags$div(
            class = "col-md-6",
            shiny::textInput(ns(paste0("footnote_url_", footnote_number)), paste0("Footnote ", footnote_number, " URL"), width = "100%")
          )
        )
      })
    })

    output$result <- renderUI({
      req(input$num_footnotes)

      my_list <- lapply(1:input$num_footnotes, function(footnote_number) {
        footnote_url <- input[[paste0("footnote_url_", footnote_number)]]
        footnote_text <- input[[paste0("footnote_text_", footnote_number)]]

        if (is.null(footnote_url) | is.null(footnote_text)) {
          return()
        }

        if (footnote_url == "") {
          list(text = footnote_text)
        } else {
          list(
            text = footnote_text,
            url = footnote_url
          )
        }
      })

      add_page_foot(
        my_list
      )
    })

    output$generated_code <- renderPrint({
      footnotes_combined <- ""

      for (footnote_number in 1:input$num_footnotes) {
        footnote_url <- input[[paste0("footnote_url_", footnote_number)]]
        footnote_text <- input[[paste0("footnote_text_", footnote_number)]]

        if (is.null(footnote_url) | is.null(footnote_text)) {
          return()
        }

        footnotes_combined <- paste0(footnotes_combined, "list(text = \"", footnote_text, "\"")

        if (footnote_url != "") {
          footnotes_combined <- paste0(footnotes_combined, ", url = \"", footnote_url, "\"")
        }

        footnotes_combined <- paste0(footnotes_combined, ")")

        if (footnote_number < input$num_footnotes) {
          footnotes_combined <- paste0(footnotes_combined, ",\n")
        }
      }

      list(text = "")
      list(text = "", url = "")

      cat(
        "add_page_foot(",
        footnotes_combined,
        "),",
        sep = "\n"
      )
    })
  })
}

## To be copied in the UI
# mod_page_foot_ui("page-foot")

## To be copied in the server
# mod_page_foot_server("page-foot")
