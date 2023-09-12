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
      title = "Top Section Input",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
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

  observeEvent(input$examine_json, {
    # if (isolate(is.null(input$file_in))) {
    #   return("Please upload a JSON file.")
    # }

    # Read the data from the json file
    # json_data <- rjson::fromJSON(file = isolate(input$file_in$datapath))
    json_data <- rjson::fromJSON(file = "P:\\1887Building\\Epidemiology\\Dashboards\\HealthEquityDashboard\\data-raw\\chronic-disease.json")

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
}
