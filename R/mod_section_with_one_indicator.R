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
        shiny::textInput(ns("main_indicator"), tags$h3("Indicator", class = "m-0")),
        shiny::textInput(ns("main_finding"), tags$h3("Main Finding", class = "m-0")),
        shiny::selectInput(ns("bg_color"), "Background color:", c("bg-water", "bg-white")),
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-4",
            shiny::sliderInput(ns("num_paragraphs"), "Number of paragraphs:", 1, 10, 1, 1, width = "100%"),
            shiny::uiOutput(ns("paragraph_inputs"))
          ),
          tags$div(
            class = "col-md-8",
            shiny::sliderInput(ns("num_graphs"), "How many section graphs are there?", 1, 10, 1, 1, width = "100%"),
            shiny::uiOutput(ns("graph_inputs"))
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
        shiny::textAreaInput(ns(paste0("paragraph_", paragraph_number)), paste0("Paragraph ", paragraph_number, " Text"), width = "100%")
      })
    })

    output$graph_inputs <- shiny::renderUI({
      req(input$num_graphs)

      lapply(1:input$num_graphs, function(graph_number) {
        tags$div(
          class = "row gx-3",
          tags$div(
            class = "col-3",
            shiny::textInput(ns(paste0("id", graph_number)), paste0("Graph ", graph_number, " ID"), width = "100%", value = letters[graph_number])
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(paste0("finding", graph_number)), paste0("Graph ", graph_number, " Finding"), width = "100%")
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(paste0("title", graph_number)), paste0("Graph ", graph_number, " Title"), width = "100%")
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(paste0("footnote", graph_number)), paste0("Graph ", graph_number, " Footnote"), width = "100%")
          )
        )
      })
    })

    output$preview <- shiny::renderUI({
      tagList(
        tags$h2("Preview"),
        add_section_with_one_indicator(
          ns = ns,
          indicator = input$main_indicator,
          description = lapply(1:input$num_paragraphs, function(paragraph_number) {
            input[[paste0("paragraph_", paragraph_number)]]
          }),
          bg_color = input$bg_color,
          main_finding = input$main_finding,
          graphs = lapply(1:input$num_graphs, function(graph_number) {
            list(
              id = input[[paste0("id", graph_number)]],
              finding = input[[paste0("finding", graph_number)]],
              title = input[[paste0("title", graph_number)]],
              footnote = input[[paste0("footnote", graph_number)]]
            )
          })
        )
      )
    })

    output$generated_code <- renderPrint({
      description_args <- ""
      for (paragraph_number in 1:input$num_paragraphs) {
        description_args <- paste0(description_args, "    \"", input[[paste0("paragraph_", paragraph_number)]], "\"")

        if (paragraph_number < input$num_paragraphs) {
          description_args <- paste0(description_args, ",\n")
        }
      }

      graph_args <- ""
      for (graph_number in 1:input$num_graphs) {
        graph_args <- paste0(
          graph_args,
          "    list(\n",
          "      id = \"", format_id(input[[paste0("id", graph_number)]]), "\",\n",
          "      finding = \"", input[[paste0("finding", graph_number)]], "\",\n",
          "      title = \"", input[[paste0("title", graph_number)]], "\",\n",
          "      footnote = \"", input[[paste0("footnote", graph_number)]], "\"\n",
          "    )"
        )

        if (graph_number < input$num_graphs) {
          graph_args <- paste0(graph_args, ",\n")
        }
      }

      cat(
        paste0("# ", input$main_indicator, " section"),
        "add_section_with_one_indicator(",
        "  ns = ns,",
        paste0("  indicator = \"", input$main_indicator, "\","),
        paste0("  description = list("),
        description_args,
        "  )",
        paste0("  bg_color = \"", input$bg_color, "\""),
        paste0("  main_finding = \"", input$main_finding, "\""),
        "  list(",
        graph_args,
        "  )",
        "),",
        sep = "\n"
      )
    })
  })
}

## To be copied in the UI
# mod_section_with_one_indicator_ui("one-indicator")

## To be copied in the server
# mod_section_with_one_indicator_server("one-indicator")
