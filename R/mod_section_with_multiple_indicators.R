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

    # Global reactive values for the server
    MAX_INDICATORS <- 5
    MAX_PARAGRAPHS <- 5
    MAX_GRAPHS <- 5

    # List of observeEvents
    obsList <- list()

    #### Reactives ####
    rv_indicators <- reactiveValues()
    rv_num_paragraphs <- reactiveValues()
    rv_paragraphs <- reactiveValues()
    rv_graph_data <- reactiveValues()

    #### Observers ####
    observeEvent(input$num_indicators, {
      req(input$num_indicators)

      # Store the previous value of the textInputs for the paragraphs, pseudo-caching
      for (outer_index in 1:MAX_INDICATORS) {
        # Store the textInput field containing the indicator for the block
        rv_indicators[[letters[outer_index]]] <- input[[paste0("indicator", outer_index)]]

        # Store the sliderInput value for how many textAreaInput fields to render for the block
        rv_num_paragraphs[[letters[outer_index]]] <- input[[paste0("num_paragraphs", outer_index)]]

        # Store the value of all textAreaInput fields for the block
        for (inner_index in 1:MAX_PARAGRAPHS) {
          rv_paragraphs[[paste0(letters[outer_index], inner_index)]] <- input[[paste0("paragraph_", letters[outer_index], inner_index)]]
        }
      }
    })

    observeEvent(input$num_graphs, {
      # Store the values for each graph input block
      for (index in 1:MAX_GRAPHS) {
        rv_graph_data$id[[letters[index]]] <- input[[paste0("id", index)]]
        rv_graph_data$finding[[letters[index]]] <- input[[paste0("finding", index)]]
        rv_graph_data$title[[letters[index]]] <- input[[paste0("title", index)]]
        rv_graph_data$footnote[[letters[index]]] <- input[[paste0("footnote", index)]]
        rv_graph_data$source[[letters[index]]] <- input[[paste0("data_source", index)]]
        rv_graph_data$axis_title[[letters[index]]] <- input[[paste0("y_title", index)]]
        rv_graph_data$axis_format[[letters[index]]] <- input[[paste0("y_format", index)]]
        rv_graph_data$hover_format[[letters[index]]] <- input[[paste0("y_hover", index)]]
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

      if (section_selection() != "Section with Multiple Indicators") {
        return()
      }

      tagList(
        tags$div(
          shiny::textInput(ns("main_indicator"), tags$h3("Main Indicator", class = "m-0")),
        shiny::textInput(ns("main_finding"), tags$h3("Main Finding", class = "m-0")),
          shiny::selectInput(ns("bg_color"), "Background color:", c("bg-water", "bg-white")),
          shiny::sliderInput(ns("num_indicators"), "Number of indicators:", 1, MAX_INDICATORS, 1, width = "100%"),
          tags$div(
            class = "row g-3",
            shiny::uiOutput(ns("indicator_inputs"))
          ),
          shiny::sliderInput(ns("num_graphs"), "Number of graphs:", 1, MAX_GRAPHS, 1, width = "100%"),
          shiny::uiOutput(ns("graph_inputs")),
          tags$h2("Preview"),
          shiny::uiOutput(ns("preview")),
          tags$h2("UI Code"),
          shiny::verbatimTextOutput(ns("generated_code_UI")),
          tags$h2("Server Code"),
          shiny::verbatimTextOutput(ns("generated_code_Server"))
        )
      )
    })

    output$indicator_inputs <- shiny::renderUI({
      req(input$num_indicators)

      # Create observeEvent calls for each sliderInput in each indicator block
      lapply(1:input$num_indicators, function(outer_index) {
        # Check if the observeEvent has already been created
        if (is.null(obsList[[letters[outer_index]]])) {
          # Create and add the observeEvent call to the obsList
          obsList[[letters[outer_index]]] <<- observeEvent(input[[paste0("num_paragraphs", outer_index)]], {
            # When the slider value is changed, store the value of all textAreaInput fields for the block
            for (inner_index in 1:MAX_PARAGRAPHS) {
              rv_paragraphs[[paste0(letters[outer_index], inner_index)]] <- input[[paste0("paragraph_", letters[outer_index], inner_index)]]
            }

            # Call the render function in each indicator block
            output[[paste0("paragraphs", outer_index)]] <- shiny::renderUI({
              # Loop through and render the appropriate number of textAreaInputs based on the slider
              lapply(1:input[[paste0("num_paragraphs", outer_index)]], function(inner_index) {
                # Create a textAreaInput field with a unique, dynamically created ID
                shiny::textAreaInput(
                  inputId = ns(paste0("paragraph_", letters[outer_index], inner_index)),
                  label = paste0("Paragraph ", inner_index),
                  value = rv_paragraphs[[paste0(letters[outer_index], inner_index)]],
                  width = "100%"
                )
              })
            })
          })
        }
      })

      tags$div(
        class = "row g-3",
        # Create the appropriate number of indicator sections based on slider input
        lapply(1:input$num_indicators, function(index) {
          tagList(
            tags$div(
              class = "col-md-6",
              tags$div(
                class = "border bg-light my-2 p-2",
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-sm-8",
                    # Create a text input for the indicator name
                    shiny::textInput(
                      inputId = ns(paste0("indicator", index)),
                      label = paste0("Indicator ", index),
                      value = rv_indicators[[letters[index]]],
                      width = "100%"
                    )
                  ),
                  tags$div(
                    class = "col-sm-4",
                    # Create a slider for the number of paragraphs the indicator will have
                    shiny::sliderInput(
                      inputId = ns(paste0("num_paragraphs", index)),
                      label = "Number of paragraphs:",
                      min = 1,
                      max = MAX_PARAGRAPHS,
                      value = ifelse(is.null(rv_num_paragraphs[[letters[index]]]), 1, rv_num_paragraphs[[letters[index]]]),
                      step = 1,
                      width = "100%"
                    )
                  )
                ),
                # Create a dynamic number of textAreaInputs for paragraph input based on slider input
                shiny::uiOutput(ns(paste0("paragraphs", index)))
              )
            )
          )
        })
      )
    })

    output$graph_inputs <- shiny::renderUI({
      req(input$num_graphs)

      lapply(1:input$num_graphs, function(index) {
        tags$div(
          class = "row gx-3 border my-1",
          tags$div(
            class = "col-3",
            shiny::textInput(ns(paste0("id", index)), paste0("Graph ", index, " ID"), width = "100%", value = ifelse(is.null(rv_graph_data$id[[letters[index]]]), letters[index], rv_graph_data$id[[letters[index]]]))
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(paste0("finding", index)), paste0("Graph ", index, " Finding"), width = "100%", value = rv_graph_data$finding[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(paste0("title", index)), paste0("Graph ", index, " Title"), width = "100%", value = rv_graph_data$title[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textAreaInput(ns(paste0("footnote", index)), paste0("Graph ", index, " Footnote"), width = "100%", value = rv_graph_data$footnote[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(paste0("data_source", index)), paste0("Graph ", index, " Data Source"), width = "100%", value = rv_graph_data$source[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(paste0("y_title", index)), paste0("Graph ", index, " Y-Axis Title"), width = "100%", value = rv_graph_data$axis_title[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(paste0("y_format", index)), paste0("Graph ", index, " Y-Axis Format"), width = "100%", value = rv_graph_data$axis_format[[letters[index]]])
          ),
          tags$div(
            class = "col-3",
            shiny::textInput(ns(paste0("y_hover", index)), paste0("Graph ", index, " Hover Format"), width = "100%", value = ifelse(is.null(rv_graph_data$hover_format[[letters[index]]]), ".1f", rv_graph_data$hover_format[[letters[index]]]))
          )
        )
      })
    })

    output$preview <- shiny::renderUI({
      tagList(
        add_body(
          ns = ns,
          main_indicator = input$main_indicator,
          sub_indicators = lapply(1:input$num_indicators, function(outer_index) {
            list(
              indicator = input[[paste0("indicator", outer_index)]],
              description = lapply(1:input[[paste0("num_paragraphs", outer_index)]], function(inner_index) {
                input[[paste0("paragraph_", letters[outer_index], inner_index)]]
              })
            )
          }),
          bg_color = input$bg_color,
          main_finding = input$main_finding,
          graphs = lapply(1:input$num_graphs, function(index) {
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
  })
}

## To be copied in the UI
# mod_section_with_multiple_indicators_ui("multi-indicator")

## To be copied in the server
# mod_section_with_multiple_indicators_server("multi-indicator")
