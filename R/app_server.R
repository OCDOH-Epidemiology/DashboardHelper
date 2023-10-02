#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  #### Constants
  MAX_HEADER_PARAGRAPHS <- 3
  MAX_BODY_SECTIONS <- MAX_HEADER_BUTTONS <- 5 # Max sections should always be the same as max buttons
  MAX_BODY_SECTION_SUBINDICATORS <- 3
  MAX_BODY_SECTION_PARAGRAPHS <- MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS <- 5 # Max section paragraphs should be the same as max subindicator paragraphs
  MAX_BODY_SECTION_GRAPHS <- 10
  MAX_FOOTER_FOOTNOTES <- 10

  output$head <- renderUI({
    shinydashboard::box(
      id = "head_section",
      title = "Page Header Input",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = "100%",
      shiny::textInput("head-indicator", label = "Main Indicator"),
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-8",
          shiny::sliderInput("head-number-of-paragraphs", "Number of paragraphs", 1, MAX_HEADER_PARAGRAPHS, 1, 1),
          lapply(1:MAX_HEADER_PARAGRAPHS, function(i) {
            shiny::textAreaInput(paste0("head-paragraph-", i), paste0("Header Paragraph ", i)) %>%
              shinyjs::hidden()
          })
        ),
        tags$div(
          class = "col-md-4",
          shiny::sliderInput("head-number-of-buttons", "Number of buttons", 1, MAX_HEADER_BUTTONS, 1, 1),
          lapply(1:MAX_HEADER_BUTTONS, function(i) {
            shiny::textInput(paste0("head-button-", i), paste0("Header Button ", i)) %>%
              shinyjs::hidden()
          })
        )
      )
    )
  })

  output$body <- renderUI({
    lapply(1:MAX_BODY_SECTIONS, function(i) { # For each section
      observeEvent(input[[paste0("section-number-of-subindicators", i)]], { # Create an observeEvent call for the slider in that section
        num_subindicators <- input[[paste0("section-number-of-subindicators", i)]] # Store the value of the slider in the section

        for (j in 1:MAX_BODY_SECTION_SUBINDICATORS) { # For each subindicator block
          if (j <= num_subindicators) {
            shinyjs::showElement(paste0("section-subindicator-block-", i, j), anim = TRUE, time = 0.5) # Show the subindicator block
          } else {
            shinyjs::hideElement(paste0("section-subindicator-block-", i, j), anim = TRUE, time = 0.5) # Hide the subindicator block
          }
        }
      })
    })

    lapply(1:MAX_BODY_SECTIONS, function(i) { # For each section
      observeEvent(input[[paste0("section-number-of-paragraphs", i)]], { # Create an observeEvent call for the slider controlling the number of paragraphs for a section
        num_paragraphs <- input[[paste0("section-number-of-paragraphs", i)]] # Store the value of the slider in the section

        for (j in 1:MAX_BODY_SECTION_PARAGRAPHS) { # For each paragraph input
          if (j <= num_paragraphs) {
            shinyjs::showElement(paste0("section-paragraph-", i, j), anim = TRUE, time = 0.5) # Show the paragraph input
          } else {
            shinyjs::hideElement(paste0("section-paragraph-", i, j), anim = TRUE, time = 0.5) # Hide the paragraph input
          }
        }
      })
    })

    lapply(1:MAX_BODY_SECTIONS, function(i) { # For each section
      lapply(1:MAX_BODY_SECTION_SUBINDICATORS, function(j) { # For each subindicator block
        observeEvent(input[[paste0("section-number-of-subindicator-paragraphs-", i, j)]], { # Create an observeEvent call for the slider in that subindicator block
          num_subindicator_paragraphs <- input[[paste0("section-number-of-subindicator-paragraphs-", i, j)]]

          for (k in 1:MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS) { # For each subindicator block
            if (k <= num_subindicator_paragraphs) {
              shinyjs::showElement(paste0("section-subindicator-paragraph-", i, j, k), anim = TRUE, time = 0.5) # Show the subindicator paragraph textAreaInput
            } else {
              shinyjs::hideElement(paste0("section-subindicator-paragraph-", i, j, k), anim = TRUE, time = 0.5) # Hide the subindicator paragraph textAreaInput
            }
          }
        })
      })
    })

    lapply(1:MAX_BODY_SECTIONS, function(i) { # For each section
      observeEvent(input[[paste0("section-number-of-graphs-", i)]], { # Create an observeEvent call for the slider controlling the number of graphs in a section
        num_graphs <- input[[paste0("section-number-of-graphs-", i)]]

        for (j in 1:MAX_BODY_SECTION_GRAPHS) { # For each paragraph input
          if (j <= num_graphs) {
            shinyjs::showElement(paste0("section-graph-inputs", i, j), anim = TRUE, time = 0.5) # Show the paragraph input
          } else {
            shinyjs::hideElement(paste0("section-graph-inputs", i, j), anim = TRUE, time = 0.5) # Hide the paragraph input
          }
        }
      })
    })

    shiny::tagList(
      shiny::sliderInput("number-of-sections", "Number of sections", 1, MAX_BODY_SECTIONS, 1, 1, width = "100%"),
      shinydashboard::box(
        id = "body-section",
        title = "Content Input",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        lapply(1:MAX_BODY_SECTIONS, function(i) {
          shinyjs::hidden(
            tags$div(
              id = paste0("section-", i),
              shinydashboard::box(
                title = paste0("Section ", i),
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                shiny::textInput(paste0("section-indicator-", i), "Section Indicator"),
                shiny::textInput(paste0("section-main-finding-", i), "Main Finding (Optional)"),
                shinydashboard::box(
                  title = "Indicator Description Paragraphs (Optional)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  shiny::sliderInput(paste0("section-number-of-paragraphs", i), "Number of paragraphs", 0, MAX_BODY_SECTION_PARAGRAPHS, 0, 1),
                  lapply(1:MAX_BODY_SECTION_PARAGRAPHS, function(j) {
                    shinyjs::hidden(shiny::textAreaInput(paste0("section-paragraph-", i, j), paste0("Paragraph ", j)))
                  })
                ),
                shinydashboard::box(
                  title = "Section Subindicators Paragraphs (Optional)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  shiny::sliderInput(paste0("section-number-of-subindicators", i), "Number of subindicators", 0, MAX_BODY_SECTION_SUBINDICATORS, 0, 1),
                  tags$div(
                    class = "row",
                    lapply(1:MAX_BODY_SECTION_SUBINDICATORS, function(j) {
                      tags$div(
                        id = paste0("section-subindicator-block-", i, j),
                        class = "col-md-4",
                        shinydashboard::box(
                          status = "warning",
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          shiny::textInput(paste0("section-subindicator-", i, j), "Indicator"),
                          shiny::sliderInput(paste0("section-number-of-subindicator-paragraphs-", i, j), "Number of paragraphs", 1, MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS, 1, 1),
                          lapply(1:MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS, function(k) {
                            shiny::textAreaInput(paste0("section-subindicator-paragraph-", i, j, k), paste0("Paragraph ", k), width = "100%")
                          })
                        )
                      )
                    })
                  )
                ),
                shiny::sliderInput(paste0("section-number-of-graphs-", i), "Number of Graphs in Section", 1, MAX_BODY_SECTION_GRAPHS, 1, 1),
                lapply(1:MAX_BODY_SECTION_GRAPHS, function(j) {
                  shinyjs::hidden(
                    tags$div(
                      class = "w-100",
                      id = paste0("section-graph-inputs", i, j),
                      shinydashboard::box(
                        width = 12,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        solidHeader = TRUE,
                        status = "warning",
                        title = paste0("Section ", i, "; Graph ", j),
                        tags$div(
                          class = "row",
                          tags$div(class = "col-md-3", shiny::textInput(paste0("excel-sheet-name", i, j), paste0("Excel Sheet Name"))),
                          tags$div(class = "col-md-3", shiny::textInput(paste0("graph-finding-", i, j), paste0("Finding"))),
                          tags$div(class = "col-md-3", shiny::textInput(paste0("graph-title-", i, j), paste0("Title"))),
                          tags$div(class = "col-md-3", shiny::textInput(paste0("graph-footnote-", i, j), paste0("Footnote"))),
                          tags$div(class = "col-md-3", shiny::textInput(paste0("graph-y-title-", i, j), paste0("Y-Axis Title"))),
                          tags$div(class = "col-md-3", shiny::textInput(paste0("graph-y-format-", i, j), paste0("Y-Axis Number Format")), value = ",0"),
                          tags$div(class = "col-md-3", shiny::textInput(paste0("graph-hover-format-", i, j), paste0("Hover Format")), value = ".1f")
                        )
                      )
                    )
                  )
                })
              )
            )
          )
        })
      )
    )
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
      shiny::sliderInput("number-of-footnotes", "Number of footnotes", 0, MAX_FOOTER_FOOTNOTES, 0, 1),
      lapply(1:MAX_FOOTER_FOOTNOTES, function(i) {
        tags$div(
          id = paste0("footnote-", i),
          class = "row",
          tags$div(class = "col-sm-6", shiny::textInput(paste0("footnote-text-", i), paste0("Footnote Text ", i))),
          tags$div(class = "col-sm-6", shiny::textInput(paste0("footnote-url-", i), paste0("Footnote URL ", i)))
        )
      })
    )
  })

  output[["page-foot"]] <- renderUI({
    tags$div(
      class = "fixed-bottom bg-outer-space",
      shiny::actionButton("update-preview", "Update Preview", class = "float-end m-2"),
      shiny::downloadButton("download-json", class = "float-end m-2")
    )
  })

  output[["download-json"]] <- shiny::downloadHandler(
    filename = "generated_file.json",
    content = function(file) {
      req(input$file_in)

      exportFile <- generate_list_from_inputs(input)

      write(jsonlite::toJSON(exportFile, pretty = TRUE), file)
    }
  )

  data_to_preview <- reactiveVal()


  observeEvent(input[["update-preview"]], {
    data_from_inputs <- generate_list_from_inputs(input)
    data_to_preview(data_from_inputs)
  })

  output$preview <- renderUI({
    data <- data_to_preview()

    if (is.null(data)) {
      return(NULL) # Initial state, nothing to show
    }

    print("Updating preview!")

    tagList(
      create_header(data$head),
      create_body(data$body),
      create_footer(data$foot)
    )
  })


  observeEvent(input$examine_json, {
    if (isolate(is.null(input$file_in))) {
      shinyjs::alert("Please upload a JSON file.")
      return()
    }

    isolate({
      # Reset head inputs
      shiny::updateTextInput(session, "head-indicator", value = "")
      shiny::updateSliderInput(session, "head-number-of-paragraphs", value = 1)
      shiny::updateSliderInput(session, "head-number-of-buttons", value = 1)
      for (i in 1:MAX_HEADER_PARAGRAPHS) {
        shiny::updateTextAreaInput(session, paste0("head-paragraph-", i), value = "")
      }
      for (i in 1:MAX_HEADER_BUTTONS) {
        shiny::updateTextInput(session, paste0("head-button-", i), value = "")
      }

      # Reset body inputs
      shiny::updateSliderInput(session, "number-of-sections", value = 1)
      for (i in 1:MAX_BODY_SECTIONS) {
        shiny::updateTextInput(session, paste0("section-indicator-", i), value = "")
        shiny::updateTextInput(session, paste0("section-main-finding-", i), value = "")

        shiny::updateSliderInput(session, paste0("section-number-of-paragraphs", i), value = 0)
        for (j in 1:MAX_BODY_SECTION_PARAGRAPHS) {
          shiny::updateTextAreaInput(session, paste0("section-paragraph-", i, j), value = "")
        }

        shiny::updateSliderInput(session, paste0("section-number-of-subindicators", i), value = 0)
        for (j in 1:MAX_BODY_SECTION_SUBINDICATORS) {
          shiny::updateTextInput(session, paste0("section-subindicator-", i, j), value = "")
          shiny::updateSliderInput(session, paste0("section-number-of-subindicator-paragraphs-", i, j), value = 1)
          for (k in 1:MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS) {
            shiny::updateTextAreaInput(session, paste0("section-subindicator-paragraph-", i, j, k), value = "")
          }
        }

        shiny::updateSliderInput(session, paste0("section-number-of-graphs-", i), value = 1)
        for (j in 1:MAX_BODY_SECTION_GRAPHS) {
          shiny::updateTextInput(session, paste0("excel-sheet-name", i, j), value = "")
          shiny::updateTextInput(session, paste0("graph-finding-", i, j), value = "")
          shiny::updateTextInput(session, paste0("graph-title-", i, j), value = "")
          shiny::updateTextInput(session, paste0("graph-footnote-", i, j), value = "")
          shiny::updateTextInput(session, paste0("graph-y-title-", i, j), value = "")
          shiny::updateTextInput(session, paste0("graph-y-format-", i, j), value = ",0")
          shiny::updateTextInput(session, paste0("graph-hover-format-", i, j), value = ".1f")
        }
      }



      # Reset foot inputs
      shiny::updateSliderInput(session, "number-of-footnotes", value = 0)
      for (i in 1:MAX_FOOTER_FOOTNOTES) {
        shiny::updateTextInput(session, paste0("footnote-text-", i), value = "")
        shiny::updateTextInput(session, paste0("footnote-url-", i), value = "")
      }

      # Read the data from the json file
      json_data <- rjson::fromJSON(file = isolate(input$file_in$datapath))
      # json_data <- rjson::fromJSON(file = "P:\\1887Building\\Epidemiology\\Dashboards\\HealthEquityDashboard\\data-raw\\chronic-disease.json")

      # Set head inputs based on json_data
      shiny::updateTextInput(session, "head-indicator", value = json_data$head$indicator)
      shiny::updateSliderInput(session, "head-number-of-paragraphs", value = length(json_data$head$paragraphs))
      shiny::updateSliderInput(session, "head-number-of-buttons", value = length(json_data$head$buttons))
      for (i in 1:length(json_data$head$paragraphs)) {
        shiny::updateTextAreaInput(session, paste0("head-paragraph-", i), value = json_data$head$paragraphs[[i]])
      }
      for (i in 1:length(json_data$head$buttons)) {
        shiny::updateTextInput(session, paste0("head-button-", i), value = json_data$head$buttons[[i]])
      }

      # Set body inputs based on json_data
      shiny::updateSliderInput(session, "number-of-sections", value = length(json_data$body))
      for (i in 1:length(json_data$body)) {
        shiny::updateTextInput(session, paste0("section-indicator-", i), value = json_data$body[[i]]$indicator)
        shiny::updateTextInput(session, paste0("section-main-finding-", i), value = json_data$body[[i]]$main_finding)

        shiny::updateSliderInput(session, paste0("section-number-of-paragraphs", i), value = length(json_data$body[[i]]$paragraphs))
        # Check if there are paragraphs, if there are, fill the subindicator boxes
        if (length(json_data$body[[i]]$paragraphs) > 0) {
          for (j in 1:length(json_data$body[[i]]$paragraphs)) {
            shiny::updateTextAreaInput(session, paste0("section-paragraph-", i, j), value = json_data$body[[i]]$paragraphs[[j]])
          }
        }

        shiny::updateSliderInput(session, paste0("section-number-of-subindicators", i), value = length(json_data$body[[i]]$subindicators))
        # Check if there are subindicators, if there are, fill the subindicator boxes
        if (length(json_data$body[[i]]$subindicators) > 0) {
          for (j in 1:length(json_data$body[[i]]$subindicators)) {
            shiny::updateTextInput(session, paste0("section-subindicator-", i, j), value = json_data$body[[i]]$subindicators[[j]]$indicator)
            shiny::updateSliderInput(session, paste0("section-number-of-subindicator-paragraphs-", i, j), value = length(json_data$body[[i]]$subindicators[[j]]$paragraphs))
            for (k in 1:length(json_data$body[[i]]$subindicators[[j]]$paragraphs)) {
              shiny::updateTextAreaInput(session, paste0("section-subindicator-paragraph-", i, j, k), value = json_data$body[[i]]$subindicators[[j]]$paragraphs[[k]])
            }
          }
        }

        # Set the values for the graph inputs
        shiny::updateSliderInput(session, paste0("section-number-of-graphs-", i), value = length(json_data$body[[i]]$graph_data))
        for (j in 1:length(json_data$body[[i]]$graph_data)) {
          current_graph_data <- json_data$body[[i]]$graph_data[[j]]

          shiny::updateTextInput(session, paste0("excel-sheet-name", i, j), value = gsub("-", " ", current_graph_data$id))
          shiny::updateTextInput(session, paste0("graph-finding-", i, j), value = ifelse(is.null(current_graph_data$finding), "", current_graph_data$finding))
          shiny::updateTextInput(session, paste0("graph-title-", i, j), value = current_graph_data$title)
          shiny::updateTextInput(session, paste0("graph-footnote-", i, j), value = current_graph_data$footnote)
          shiny::updateTextInput(session, paste0("graph-y-title-", i, j), value = current_graph_data$y_title)
          shiny::updateTextInput(session, paste0("graph-y-format-", i, j), value = current_graph_data$y_format)
          shiny::updateTextInput(session, paste0("graph-hover-format-", i, j), value = current_graph_data$hover_format)
        }
      }

      # Set foot inputs based on json_data
      shiny::updateSliderInput(session, "number-of-footnotes", value = length(json_data$foot))
      if (length(json_data$foot) > 0) {
        for (i in 1:length(json_data$foot)) {
          shiny::updateTextInput(session, paste0("footnote-text-", i), value = json_data$foot[[i]]$text)
          shiny::updateTextInput(session, paste0("footnote-url-", i), value = ifelse(is.null(json_data$foot[[i]]$url), "", json_data$foot[[i]]$url))
        }
      }
    })
  })

  observeEvent(input[["head-number-of-paragraphs"]], {
    for (i in 1:MAX_HEADER_PARAGRAPHS) {
      if (i <= input[["head-number-of-paragraphs"]]) {
        shinyjs::showElement(paste0("head-paragraph-", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("head-paragraph-", i), anim = TRUE, time = 0.5)
      }
    }
  })

  observeEvent(input[["head-number-of-buttons"]], {
    for (i in 1:MAX_HEADER_BUTTONS) {
      if (i <= input[["head-number-of-buttons"]]) {
        shinyjs::showElement(paste0("head-button-", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("head-button-", i), anim = TRUE, time = 0.5)
      }
    }
  })

  observeEvent(input[["number-of-sections"]], {
    for (i in 1:MAX_BODY_SECTIONS) {
      if (i <= input[["number-of-sections"]]) {
        shinyjs::showElement(paste0("section-", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("section-", i), anim = TRUE, time = 0.5)
      }
    }
  })

  observeEvent(input[["number-of-footnotes"]], {
    for (i in 1:MAX_FOOTER_FOOTNOTES) {
      if (i <= input[["number-of-footnotes"]]) {
        shinyjs::showElement(paste0("footnote-", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("footnote-", i), anim = TRUE, time = 0.5)
      }
    }
  })
}
