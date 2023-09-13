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
  MAX_HEADER_BUTTONS <- 5
  MAX_BODY_SECTIONS <- MAX_HEADER_BUTTONS
  MAX_BODY_SECTION_SUBINDICATORS <- 3
  MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS <- 5
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
      shiny::textInput("head_indicator", label = "Main Indicator"),
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-8",
          shiny::sliderInput("head_num_desc", "Number of paragraphs", 1, MAX_HEADER_PARAGRAPHS, 1, 1),
          lapply(1:MAX_HEADER_PARAGRAPHS, function(x) {
            shiny::textAreaInput(paste0("header_paragraph", x), paste0("Header Paragraph ", x)) %>%
              shinyjs::hidden()
          })
        ),
        tags$div(
          class = "col-md-4",
          shiny::sliderInput("head_num_button", "Number of buttons", 1, MAX_HEADER_BUTTONS, 1, 1),
          lapply(1:MAX_HEADER_BUTTONS, function(x) {
            shiny::textInput(paste0("header_button", x), paste0("Header Button ", x)) %>%
              shinyjs::hidden()
          })
        )
      )
    )
  })

  output$body <- renderUI({
    lapply(1:MAX_BODY_SECTIONS, function(i) { # For each section
      observeEvent(input[[paste0("num-subindicators-", i)]], { # Create an observeEvent call for the slider in that section
        num_subindicators <- input[[paste0("num-subindicators-", i)]] # Store the value of the slider in the section

        for (j in 1:MAX_BODY_SECTION_SUBINDICATORS) { # For each subindicator block
          if (j <= num_subindicators) {
            shinyjs::showElement(paste0("subindicator-block-", i, j), anim = TRUE, time = 0.5) # Show the subindicator block
          } else {
            shinyjs::hideElement(paste0("subindicator-block-", i, j), anim = TRUE, time = 0.5) # Hide the subindicator block
          }
        }
      })
    })

    lapply(1:MAX_BODY_SECTIONS, function(i) { # For each section
      lapply(1:MAX_BODY_SECTION_SUBINDICATORS, function(j) { # For each subindicator block
        observeEvent(input[[paste0("subindicator-paragraph-slider-", i, j)]], { # Create an observeEvent call for the slider in that subindicator block
          num_subindicator_paragraphs <- input[[paste0("subindicator-paragraph-slider-", i, j)]]

          for (k in 1:MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS) { # For each subindicator block
            if (k <= num_subindicator_paragraphs) {
              shinyjs::showElement(paste0("subindicator-paragraph-", i, j, k), anim = TRUE, time = 0.5) # Show the subindicator paragraph textAreaInput
            } else {
              shinyjs::hideElement(paste0("subindicator-paragraph-", i, j, k), anim = TRUE, time = 0.5) # Hide the subindicator paragraph textAreaInput
            }
          }
        })
      })
    })

    shiny::tagList(
      shiny::sliderInput("num_sections", "Number of sections", 1, MAX_BODY_SECTIONS, 1, 1, width = "100%"),
      shinydashboard::box(
        id = "body_section",
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
                shiny::textInput(paste0("indicator-", i), "Section Indicator"),
                shiny::sliderInput(paste0("num-subindicators-", i), "Number of sub-indicators", 1, MAX_BODY_SECTION_SUBINDICATORS, 1, 1),
                tags$div(
                  class = "row",
                  lapply(1:MAX_BODY_SECTION_SUBINDICATORS, function(j) {
                    tags$div(
                      id = paste0("subindicator-block-", i, j),
                      class = "col-md-6",
                      shinydashboard::box(
                        status = "warning",
                        solidHeader = TRUE,
                        width = 12,
                        shiny::textInput(paste0("subindicator-", i, j), "Indicator"),
                        shiny::sliderInput(paste0("subindicator-paragraph-slider-", i, j), "Number of paragraphs", 1, MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS, 1, 1),
                        lapply(1:MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS, function(k) {
                          shiny::textAreaInput(paste0("subindicator-paragraph-", i, j, k), paste0("Paragraph ", k), width = "100%")
                        })
                      )
                    )
                  })
                )
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
      width = "100%"
    )
  })

  observeEvent(input$examine_json, {
    # if (isolate(is.null(input$file_in))) {
    #   return("Please upload a JSON file.")
    # }

    # Read the data from the json file
    # json_data <- rjson::fromJSON(file = isolate(input$file_in$datapath))
    json_data <- rjson::fromJSON(file = "P:\\1887Building\\Epidemiology\\Dashboards\\HealthEquityDashboard\\data-raw\\communicable-disease.json")

    # Using the data from the json file, set the values of input on the page
    shiny::updateTextInput(session, "head_indicator", value = json_data$header$indicator)
    shiny::updateSliderInput(session, "head_num_desc", value = length(json_data$header$descriptions))
    shiny::updateSliderInput(session, "head_num_button", value = length(json_data$header$sections))

    for (i in 1:length(json_data$header$descriptions)) {
      shiny::updateTextAreaInput(session, paste0("header_paragraph", i), value = json_data$header$descriptions[[i]])
    }
    for (i in 1:length(json_data$header$sections)) {
      shiny::updateTextInput(session, paste0("header_button", i), value = json_data$header$sections[[i]])
    }

    shiny::updateSliderInput(session, "num_sections", value = length(json_data$body))
    for (i in 1:length(json_data$body)) {
      shiny::updateTextInput(session, paste0("indicator-", i), value = json_data$body[[i]]$main_indicator)
      shiny::updateSliderInput(session, paste0("num-subindicators-", i), value = length(json_data$body[[i]]$subindicators))
      for (j in 1:length(json_data$body[[i]]$subindicators)) {
        shiny::updateTextInput(session, paste0("subindicator-", i, j), value = json_data$body[[i]]$subindicators[[j]]$indicator)
        shiny::updateSliderInput(session, paste0("subindicator-paragraph-slider-", i, j), value = length(json_data$body[[i]]$subindicators[[j]]$description))
        for (k in 1:length(json_data$body[[i]]$subindicators[[j]]$description)) {
          shiny::updateTextAreaInput(session, paste0("subindicator-paragraph-", i, j, k), value = json_data$body[[i]]$subindicators[[j]]$description[[k]])
        }
      }
    }
  })

  observeEvent(input$head_num_desc, {
    for (i in 1:MAX_HEADER_PARAGRAPHS) {
      if (i <= input$head_num_desc) {
        shinyjs::showElement(paste0("header_paragraph", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("header_paragraph", i), anim = TRUE, time = 0.5)
      }
    }
  })

  observeEvent(input$head_num_button, {
    for (i in 1:MAX_HEADER_BUTTONS) {
      if (i <= input$head_num_button) {
        shinyjs::showElement(paste0("header_button", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("header_button", i), anim = TRUE, time = 0.5)
      }
    }
  })

  observeEvent(input$num_sections, {
    for (i in 1:MAX_BODY_SECTIONS) {
      if (i <= input$num_sections) {
        shinyjs::showElement(paste0("section-", i), anim = TRUE, time = 0.5)
      } else {
        shinyjs::hideElement(paste0("section-", i), anim = TRUE, time = 0.5)
      }
    }
  })
}
