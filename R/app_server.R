#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  constants <- list(
    MAX_HEADER_PARAGRAPHS = 3,
    MAX_BODY_SECTIONS = 6,
    MAX_HEADER_BUTTONS = 6, # Max sections should always be the same as max buttons
    MAX_BODY_SECTION_SUBINDICATORS = 3,
    MAX_BODY_SECTION_PARAGRAPHS = 5,
    MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS = 5, # Max section paragraphs should be the same as max subindicator paragraphs
    MAX_BODY_SECTION_GRAPHS = 10,
    MAX_FOOTER_FOOTNOTES = 10
  )

  output$head <- renderUI({
    shinydashboard::box(
      id = "head_section",
      title = "Page Header Input",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = "100%",
      shiny::textInput("page_indicator", label = "Main Indicator"),
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-8",
          shiny::sliderInput("header_num_paragraphs", "Number of paragraphs", 1, constants$MAX_HEADER_PARAGRAPHS, 1, 1),
          lapply(1:constants$MAX_HEADER_PARAGRAPHS, function(i) {
            shiny::textAreaInput(paste0("header_paragraph", i), paste0("Header Paragraph ", i)) %>%
              shinyjs::hidden()
          })
        ),
        tags$div(
          class = "col-md-4",
          shiny::sliderInput("header_num_buttons", "Number of buttons", 1, constants$MAX_HEADER_BUTTONS, 1, 1),
          lapply(1:constants$MAX_HEADER_BUTTONS, function(i) {
            shiny::textInput(paste0("header_button", i), paste0("Header Button ", i)) %>%
              shinyjs::hidden()
          })
        )
      )
    )
  })

  observeEvent(input$header_num_paragraphs, {
    for (i in 1:constants$MAX_HEADER_PARAGRAPHS) {
      if (i <= input$header_num_paragraphs) {
        shinyjs::showElement(paste0("header_paragraph", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("header_paragraph", i), anim = TRUE, time = 0.5)
      }
    }
  })

  observeEvent(input$header_num_buttons, {
    # Update the slider for number of sections
    shiny::updateSliderInput(session, "num_sections", value = input$header_num_buttons)

    # Update the visibility of textInputs for the page buttons
    for (i in 1:constants$MAX_HEADER_BUTTONS) {
      if (i <= input$header_num_buttons) {
        shinyjs::showElement(paste0("header_button", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("header_button", i), anim = TRUE, time = 0.5)
      }
    }
  })

  lapply(1:constants$MAX_HEADER_BUTTONS, function(i) {
    observeEvent(input[[paste0("header_button", i)]], {
      shiny::updateTextInput(session, paste0("body_section_indicator", i), value = input[[paste0("header_button", i)]])
    })
  })

  indicator_paragraph_input_section <- function(i) {
    shinydashboard::box(
      title = "Indicator Description Paragraphs (Optional)",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::sliderInput(paste0("section_num_paragraphs", i), "Number of paragraphs", 0, constants$MAX_BODY_SECTION_PARAGRAPHS, 0, 1),
      lapply(1:constants$MAX_BODY_SECTION_PARAGRAPHS, function(j) {
        shinyjs::hidden(shiny::textAreaInput(paste0("section_paragraphs", i, j), paste0("Paragraph ", j)))
      })
    )
  }

  subindicator_input_section <- function(i) {
    shinydashboard::box( # Subindicator Input Box
      title = "Section Subindicators Paragraphs (Optional)",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::sliderInput(paste0("num_subindicators", i), "Number of subindicators", 0, constants$MAX_BODY_SECTION_SUBINDICATORS, 0, 1),
      tags$div(
        class = "row",
        lapply(1:constants$MAX_BODY_SECTION_SUBINDICATORS, function(j) {
          tags$div(
            id = paste0("subindicator_block", i, j),
            class = "col-md-4",
            shinydashboard::box(
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              shiny::textInput(paste0("subindicator", i, j), "Sub-Indicator"),
              shiny::sliderInput(paste0("subindicator_num_paragraph", i, j), "Number of paragraphs", 1, constants$MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS, 1, 1),
              lapply(1:constants$MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS, function(k) {
                shiny::textAreaInput(paste0("subindicator_paragraph", i, j, k), paste0("Paragraph ", k), width = "100%")
              })
            )
          )
        })
      )
    )
  }

  graph_input_section <- function(i) {
    lapply(1:constants$MAX_BODY_SECTION_GRAPHS, function(j) {
      shinyjs::hidden(
        tags$div(
          class = "col-xxl-4 col-lg-3 col-md-4 col-sm-6",
          id = paste0("section_graph_inputs", i, j),
          shinydashboard::box( # Input box for graph information
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            solidHeader = TRUE,
            status = "warning",
            title = paste0("Section ", i, "; Graph ", j),
            tagList(
              shiny::textInput(paste0("data_name", i, j), "Name of Data"),
              shiny::textInput(paste0("excel_sheet_name", i, j), "Excel Sheet Name"),
              shiny::textAreaInput(paste0("data_observation", i, j), "Data Observation"),
              shiny::textAreaInput(paste0("graph_title", i, j), "Graph Title"),
              shiny::textAreaInput(paste0("data_source", i, j), "Data Source"),
              shiny::textAreaInput(paste0("data_note", i, j), "Notes for Data"),
              shiny::textInput(paste0("y_axis_title", i, j), "Y-Axis Title"),
              shiny::selectizeInput(paste0("data_format_type", i, j), "Format Type", choices = c("rate", "currency", "percent"), selected = "rate"),
              shiny::dateInput(paste0("last_updated", i, j), "Date Last Updated", value = Sys.Date(), format = "mm/dd/yyyy")
            )
          )
        )
      )
    })
  }

  input_section <- function() {
    lapply(1:constants$MAX_BODY_SECTIONS, function(i) {
      shinyjs::hidden(
        tags$div(
          id = paste0("section", i),
          class = "col-xxl-6",
          shinydashboard::box(
            title = paste0("Section ", i),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            shiny::textInput(paste0("section_indicator", i), "Section Indicator"),
            indicator_paragraph_input_section(i),
            subindicator_input_section(i),
            shiny::sliderInput(paste0("section_num_graphs", i), "Number of Graphs in Section", 1, constants$MAX_BODY_SECTION_GRAPHS, 1, 1),
            tags$div(
              class = "row",
              graph_input_section(i)
            )
          )
        )
      )
    })
  }

  output$body <- renderUI({
    shiny::tagList(
      shiny::sliderInput("num_sections", "Number of sections", 1, constants$MAX_BODY_SECTIONS, 1, 1, width = "100%"),
      shinydashboard::box(
        id = "body-section",
        title = "Content Input",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        tags$div(
          class = "row",
          input_section()
        )
      )
    )
  })

  observeEvent(input$num_sections, {
    # Update the slider for number of buttons in the header
    shiny::updateSliderInput(session, "header_num_buttons", value = input$num_sections)

    # Update the visibility of boxes for inputting section info
    for (i in 1:constants$MAX_BODY_SECTIONS) {
      if (i <= input$num_sections) {
        shinyjs::showElement(paste0("section", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("section", i), anim = TRUE, time = 0.5)
      }
    }
  })

  lapply(1:constants$MAX_BODY_SECTIONS, function(i) {
    observeEvent(input[[paste0("section-indicator-", i)]], { # Create an observeEvent for each textInput for the buttons
      shiny::updateTextInput(session, paste0("head-button-", i), value = input[[paste0("section-indicator-", i)]])
    })
  })

  # ObserveEvent for toggling section subindicator block visibility
  lapply(1:constants$MAX_BODY_SECTIONS, function(i) { # For each section
    observeEvent(input[[paste0("num_subindicators", i)]], { # Create an observeEvent call for the slider in that section
      num_subindicators <- input[[paste0("num_subindicators", i)]] # Store the value of the slider in the section

      for (j in 1:constants$MAX_BODY_SECTION_SUBINDICATORS) { # For each subindicator block
        if (j <= num_subindicators) {
          shinyjs::showElement(paste0("subindicator_block", i, j), anim = TRUE, time = 0.5) # Show the subindicator block
        } else {
          shinyjs::hideElement(paste0("subindicator_block", i, j), anim = TRUE, time = 0.5) # Hide the subindicator block
        }
      }
    })
  })

  # ObserveEvent for toggling section description paragraph input visibility
  lapply(1:constants$MAX_BODY_SECTIONS, function(i) { # For each section
    observeEvent(input[[paste0("section_num_paragraphs", i)]], { # Create an observeEvent call for the slider controlling the number of paragraphs for a section
      num_paragraphs <- input[[paste0("section_num_paragraphs", i)]] # Store the value of the slider in the section

      for (j in 1:constants$MAX_BODY_SECTION_PARAGRAPHS) { # For each paragraph input
        if (j <= num_paragraphs) {
          shinyjs::showElement(paste0("section_paragraphs", i, j), anim = TRUE, time = 0.5) # Show the paragraph input
        } else {
          shinyjs::hideElement(paste0("section_paragraphs", i, j), anim = TRUE, time = 0.5) # Hide the paragraph input
        }
      }
    })
  })

  # ObserveEvent for toggling section subindicator paragraph input visibility
  lapply(1:constants$MAX_BODY_SECTIONS, function(i) { # For each section
    lapply(1:constants$MAX_BODY_SECTION_SUBINDICATORS, function(j) { # For each subindicator block
      observeEvent(input[[paste0("subindicator_num_paragraph", i, j)]], { # Create an observeEvent call for the slider in that subindicator block
        subindicator_num_paragraph <- input[[paste0("subindicator_num_paragraph", i, j)]]

        for (k in 1:constants$MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS) { # For each subindicator block
          if (k <= subindicator_num_paragraph) {
            shinyjs::showElement(paste0("subindicator_paragraph", i, j, k), anim = TRUE, time = 0.5) # Show the subindicator paragraph textAreaInput
          } else {
            shinyjs::hideElement(paste0("subindicator_paragraph", i, j, k), anim = TRUE, time = 0.5) # Hide the subindicator paragraph textAreaInput
          }
        }
      })
    })
  })

  # ObserveEvent for toggling section graph input visibility
  lapply(1:constants$MAX_BODY_SECTIONS, function(i) { # For each section
    observeEvent(input[[paste0("section_num_graphs", i)]], { # Create an observeEvent call for the slider controlling the number of graphs in a section
      num_graphs <- input[[paste0("section_num_graphs", i)]]

      for (j in 1:constants$MAX_BODY_SECTION_GRAPHS) { # For each paragraph input
        if (j <= num_graphs) {
          shinyjs::showElement(paste0("section_graph_inputs", i, j), anim = TRUE, time = 0.5) # Show the paragraph input
        } else {
          shinyjs::hideElement(paste0("section_graph_inputs", i, j), anim = TRUE, time = 0.5) # Hide the paragraph input
        }
      }
    })
  })

  output$foot <- renderUI({
    shinydashboard::box(
      id = "foot_section",
      title = "Bottom Section Input",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = "100%",
      shiny::sliderInput("num_footnotes", "Number of footnotes", 0, constants$MAX_FOOTER_FOOTNOTES, 0, 1),
      lapply(1:constants$MAX_FOOTER_FOOTNOTES, function(i) {
        tags$div(
          id = paste0("footnote", i),
          class = "row",
          tags$div(class = "col-sm-6", shiny::textInput(paste0("footnote_text", i), paste0("Footnote Text ", i))),
          tags$div(class = "col-sm-6", shiny::textInput(paste0("footnote_link", i), paste0("Footnote URL ", i)))
        )
      })
    )
  })

  output$download_json <- shiny::downloadHandler(
    filename = "generated_file.json",
    content = function(file) {
      exportFile <- generate_list_from_inputs(input)

      write(jsonlite::toJSON(exportFile, pretty = TRUE), file)
    }
  )

  data_to_preview <- reactiveVal()

  observeEvent(input$update_preview, {
    data_from_inputs <- generate_list_from_inputs(input)
    data_to_preview(data_from_inputs)
  })

  output$preview <- renderUI({
    data <- data_to_preview()

    if (is.null(data)) {
      return(NULL) # Initial state, nothing to show
    }

    print("Updating preview!")

    jsonlite::toJSON(data, pretty = TRUE)

    # tagList(
    #   create_header(data$head),
    #   create_body(data$body),
    #   create_footer(data$foot)
    # )
  })

  observeEvent(input$examine_json, {
    if (isolate(is.null(input$file_in))) {
      shinyjs::alert("Please upload a JSON file.")
      return()
    }

    isolate({
      reset_inputs(session, constants)

      # Read the data from the json file
      json_data <- rjson::fromJSON(file = isolate(input$file_in$datapath))

      # Debugging purposes only!
      # json_data <- rjson::fromJSON(file = "P:\\1887Building\\Epidemiology\\Dashboards\\HealthEquityDashboard\\data-raw\\chronic-disease.json")

      import_JSON(session, json_data)
    })
  })

  observeEvent(input$num_footnotes, {
    for (i in 1:constants$MAX_FOOTER_FOOTNOTES) {
      if (i <= input$num_footnotes) {
        shinyjs::showElement(paste0("footnote", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("footnote", i), anim = TRUE, time = 0.5)
      }
    }
  })
}
