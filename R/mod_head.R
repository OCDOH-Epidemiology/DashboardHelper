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

    #### Reactives ####
    previous_paragraphs <- reactiveValues()
    previous_buttons <- reactiveValues()

    #### Observers ####
    observeEvent(input$num_paragraphs, {
      # Store the previous value of the textInputs for the paragraphs, pseudo-caching
      for (index in 1:10) {
        paragraph_input_id <- paste0("paragraph", index)
        previous_paragraphs[[letters[index]]] <- input[[paragraph_input_id]]
      }
    })

    observeEvent(input$num_buttons, {
      # Store the previous value of the textInputs for the buttons, pseudo-caching
      for (index in 1:10) {
        button_input_id <- paste0("button", index)
        previous_buttons[[letters[index]]] <- input[[button_input_id]]
      }
    })

    #### Outputs ####
    output$page_head <- shiny::renderUI({
      req(section_selection())

      # Return early if the dropdown selection is not equal to "Head Section"
      if (section_selection() != "Head Section") {
        return()
      }

      # Generate the HTML output
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

      lapply(1:input$num_paragraphs, function(index) {
        # Dynamically generate an id for the textInput
        paragraph_input_id <- paste0("paragraph", index)

        # Dynamically create the label for the textInput
        paragraph_input_label <- paste0("Paragraph ", index, " Text")

        # Create a textInput field with the generated id and label
        shiny::textInput(ns(paragraph_input_id), paragraph_input_label, width = "100%", value = previous_paragraphs[[letters[index]]])
      })
    })

    output$button_inputs <- shiny::renderUI({
      req(input$num_buttons)

      lapply(1:input$num_buttons, function(index) {
        # Dynamically generate an id for the textInput
        button_input_id <- paste0("button", index)

        # Dynamically create the label for the textInput
        button_input_label <- paste0("Button ", index, " Text")

        # Create a textInput field with the generated id and label
        shiny::textInput(ns(button_input_id), button_input_label, width = "100%", value = previous_buttons[[letters[index]]])
      })
    })

    output$preview <- shiny::renderUI({
      # Output the preview
      tagList(
        tags$h2("Preview"),
        add_head(
          indicator = input$main_indicator,
          description = lapply(1:input$num_paragraphs, function(paragraph_number) {
            input[[paste0("paragraph", paragraph_number)]]
          }),
          sections = lapply(1:input$num_buttons, function(button_number) {
            input[[paste0("button", button_number)]]
          })
        )
      )
    })

    output$generated_code <- shiny::renderPrint({
      # Create an variable storing the opening part of the generated code
      generated_code <- paste0(
        "add_head(\n",
        "  indicator = \"", input$main_indicator, "\",\n",
        "  description = list(\n"
      )

      # Loop through all paragraph inputs
      for (index in 1:input$num_paragraphs) {
        paragraph_text <- input[[paste0("paragraph", index)]]

        # Append the paragraph text to the generated code
        generated_code <- paste0(generated_code, "    \"", paragraph_text, "\"")

        # If the current index is not the final index, append a comma to the text
        if (index < max(input$num_paragraphs)) {
          generated_code <- paste0(generated_code, ",")
        }

        # Append a new line to the end of the generated code
        generated_code <- paste0(generated_code, "\n")
      }

      # Close the description list and open the section list
      generated_code <- paste0(
        generated_code,
        "  ),\n",
        "  sections = list(\n"
      )

      # Loop through all button inputs
      for (index in 1:input$num_buttons) {
        button_text <- input[[paste0("button", index)]]

        # Append the button text to the generated code
        generated_code <- paste0(generated_code, "    \"", button_text, "\"")

        # If the current index is not the final index, append a comma to the text
        if (index < max(input$num_buttons)) {
          generated_code <- paste0(generated_code, ",")
        }

        # Append a new line to the end of the generated code
        generated_code <- paste0(generated_code, "\n")
      }

      # Close the sections list and function call
      generated_code <- paste0(
        generated_code,
        "  )\n",
        "),"
      )

      # Output the finalized code
      cat(generated_code)
    })
  })
}

## To be copied in the UI
# mod_page_head_ui("page-head")

## To be copied in the server
# mod_page_head_server("page-head")
