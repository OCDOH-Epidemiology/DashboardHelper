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

    #### Reactives ####
    previous_paragraphs <- reactiveValues()
    previous_graphs <- reactiveValues()

    #### Observers ####
    observeEvent(input$num_paragraphs, {
      # Store the previous value of the textInputs for the paragraphs, pseudo-caching
      for (index in 1:10) {
        paragraph_input_id <- paste0("paragraph", index)
        previous_paragraphs[[letters[index]]] <- input[[paragraph_input_id]]
      }
    })

    observeEvent(input$num_graphs, {
      # Store the previous value of the textInputs for the buttons, pseudo-caching
      for (index in 1:10) {
        graph_id <- paste0("id", index)
        graph_finding_id <- paste0("finding", index)
        graph_title_id <- paste0("title", index)
        graph_footnote_id <- paste0("footnote", index)
        graph_source_id <- paste0("data_source", index)
        graph_axis_title_id <- paste0("y_title", index)
        graph_axis_format_id <- paste0("y_format", index)
        graph_hover_format_id <- paste0("y_hover", index)

        previous_graphs$id[[letters[index]]] <- input[[graph_id]]
        previous_graphs$finding[[letters[index]]] <- input[[graph_finding_id]]
        previous_graphs$title[[letters[index]]] <- input[[graph_title_id]]
        previous_graphs$footnote[[letters[index]]] <- input[[graph_footnote_id]]
        previous_graphs$source[[letters[index]]] <- input[[graph_source_id]]
        previous_graphs$axis_title[[letters[index]]] <- input[[graph_axis_title_id]]
        previous_graphs$axis_format[[letters[index]]] <- input[[graph_axis_format_id]]
        previous_graphs$hover_format[[letters[index]]] <- input[[graph_hover_format_id]]
      }
    })

    observe({
      req(input$num_graphs)

      # Generate the graphs with cars data
      lapply(1:input$num_graphs, function(index) {
        id <- req(format_id(input[[paste0("id", index)]]))
        local({
          output[[id]] <- plotly::renderPlotly({
            cars %>%
              dplyr::group_by(speed) %>%
              dplyr::summarise(dist = sum(dist)) %>%
              plotly::plot_ly(x = ~speed, y = ~ dist * 10, type = "bar") %>%
              plotly::layout(
                xaxis = list(title = "", categoryorder = "trace", fixedrange = TRUE),
                yaxis = list(
                  title = input[[paste0("y_title-", index)]],
                  tickformat = input[[paste0("y_format-", index)]],
                  hoverformat = input[[paste0("y_hover-", index)]],
                  fixedrange = TRUE
                )
              )
          })
        })
      })
    })

    #### Outputs ####
    output$main <- shiny::renderUI({
      req(section_selection())

      if (section_selection() != "Section with One Indicator") {
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
        tags$h2("Preview"),
        shiny::uiOutput(ns("preview")),
        tags$h2("UI Code"),
        shiny::verbatimTextOutput(ns("generated_code_UI")),
        tags$h2("Server Code"),
        shiny::verbatimTextOutput(ns("generated_code_Server"))
      )
    })

    output$paragraph_inputs <- shiny::renderUI({
      req(input$num_paragraphs)

      lapply(1:input$num_paragraphs, function(index) {
        paragraph_input_id <- paste0("paragraph", index)
        paragraph_input_label <- paste0("Paragraph ", index, " Text")

        shiny::textAreaInput(ns(paragraph_input_id), paragraph_input_label, width = "100%", value = previous_paragraphs[[letters[index]]])
      })
    })

    output$graph_inputs <- shiny::renderUI({
      req(input$num_graphs)

      lapply(1:input$num_graphs, function(index) {
        graph_id <- paste0("id", index)
        graph_finding_id <- paste0("finding", index)
        graph_title_id <- paste0("title", index)
        graph_footnote_id <- paste0("footnote", index)
        graph_source_id <- paste0("data_source", index)
        graph_axis_title_id <- paste0("y_title", index)
        graph_axis_format_id <- paste0("y_format", index)
        graph_hover_format_id <- paste0("y_hover", index)

        graph_label <- paste0("Graph ", index, " ID")
        graph_finding_label <- paste0("Graph ", index, " Finding")
        graph_title_label <- paste0("Graph ", index, " Title")
        graph_footnote_label <- paste0("Graph ", index, " Footnote")
        graph_source_label <- paste0("Graph ", index, " Data Source")
        graph_axis_title_label <- paste0("Graph ", index, " Y-Axis Title")
        graph_axis_format_label <- paste0("Graph ", index, " Y-Axis Format")
        graph_hover_format_label <- paste0("Graph ", index, " Hover Format")

        tags$div(
          class = "row gx-3 border",
          tags$div(
            class = "col-3",
            shiny::textInput(ns(graph_id), graph_label, width = "100%", value = ifelse(is.null(previous_graphs$id[[letters[index]]]), letters[index], previous_graphs$id[[letters[index]]]))
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(graph_finding_id), graph_finding_label, width = "100%", value = previous_graphs$finding[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(graph_title_id), graph_title_label, width = "100%", value = previous_graphs$title[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(graph_footnote_id), graph_footnote_label, width = "100%", value = previous_graphs$footnote[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(graph_source_id), graph_source_label, width = "100%", value = previous_graphs$source[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(graph_axis_title_id), graph_axis_title_label, width = "100%", value = previous_graphs$axis_title[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(graph_axis_format_id), graph_axis_format_label, width = "100%", value = previous_graphs$axis_format[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(graph_hover_format_id), graph_hover_format_label, width = "100%", value = ifelse(is.null(previous_graphs$hover_format[[letters[index]]]), ".1f", previous_graphs$hover_format[[letters[index]]]))
          )
        )
      })
    })

    output$preview <- shiny::renderUI({
      # Necessary delay to ensure that graph inputs are generated before rendering the graphs
      Sys.sleep(.1)

      tagList(
        add_section_with_one_indicator(
          ns = ns,
          indicator = input$main_indicator,
          description = lapply(1:input$num_paragraphs, function(index) {
            input[[paste0("paragraph", index)]]
          }),
          bg_color = input$bg_color,
          main_finding = input$main_finding,
          graphs = lapply(1:input$num_graphs, function(index) {
            req(input[[paste0("id", index)]])
            list(
              id = input[[paste0("id", index)]],
              finding = input[[paste0("finding", index)]],
              title = input[[paste0("title", index)]],
              footnote = input[[paste0("footnote", index)]]
            )
          })
        )
      )
    })

    output$generated_code_UI <- renderPrint({
      description_args <- ""
      for (index in 1:input$num_paragraphs) {
        description_args <- paste0(description_args, "    \"", input[[paste0("paragraph", index)]], "\"")

        if (index < input$num_paragraphs) {
          description_args <- paste0(description_args, ",\n")
        }
      }

      graph_args <- ""
      for (index in 1:input$num_graphs) {
        graph_args <- paste0(
          graph_args,
          "    list(\n",
          "      id = \"", format_id(input[[paste0("id", index)]]), "\",\n",
          "      finding = \"", input[[paste0("finding", index)]], "\",\n",
          "      title = \"", input[[paste0("title", index)]], "\",\n",
          "      footnote = \"", input[[paste0("footnote", index)]], "\"\n",
          "    )"
        )

        if (index < input$num_graphs) {
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
        "  ),",
        paste0("  bg_color = \"", input$bg_color, "\","),
        paste0("  main_finding = \"", input$main_finding, "\","),
        "  list(",
        graph_args,
        "  )",
        ")",
        sep = "\n"
      )
    })

    output$generated_code_Server <- renderPrint({
      module_calls <- NULL
      for (index in 1:input$num_graphs) {
        module_calls <- paste0(
          module_calls,
          "mod_add_graph_server(\n",
          "  id = \"", format_id(input[[paste0("id", index)]]), "\",\n",
          "  data_in = \"", input[[paste0("data_source", index)]], "\",\n",
          "  y_title = \"", input[[paste0("y_title", index)]], "\",\n",
          "  y_format = \"", input[[paste0("y_format", index)]], "\",\n",
          "  y_hover = \"", input[[paste0("y_hover", index)]], "\"\n",
          ")"
        )

        if (index < input$num_graphs) {
          module_calls <- paste0(module_calls, ",\n\n")
        }
      }

      cat(
        module_calls
      )
    })
  })
}

## To be copied in the UI
# mod_section_with_one_indicator_ui("one-indicator")

## To be copied in the server
# mod_section_with_one_indicator_server("one-indicator")
