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

    #### Reactives ####
    previous_text <- reactiveValues()
    previous_urls <- reactiveValues()

    #### Observers ####
    observeEvent(input$num_footnotes, {
      # Store the previous value of the textInputs for the text and urls, pseudo-chaching
      for (index in 1:10) {
        text_input_id <- paste0("text", index)
        url_input_id <- paste0("url", index)

        previous_text[[letters[index]]] <- input[[text_input_id]]
        previous_urls[[letters[index]]] <- input[[url_input_id]]
      }
    })

    #### Outputs ####
    output$page_foot <- shiny::renderUI({
      req(section_selection())

      # Return early if the dropdown selection is not equal to "Page Footer"
      if (section_selection() != "Page Footer") {
        return()
      }

      # Generate the HTML output
      tagList(
        shiny::sliderInput(ns("num_footnotes"), "How many footnotes are there?", 1, 15, 1, 1, width = "50%"),
        shiny::uiOutput(ns("footnote_inputs")),
        tags$strong("*If the footnote is not a link, leave the URL blank."),
        shiny::uiOutput(ns("preview")),
        tags$br(),
        shiny::verbatimTextOutput(ns("generated_code"))
      )
    })

    output$footnote_inputs <- shiny::renderUI({
      req(input$num_footnotes)

      # Create a list based on the value of the slider input
      lapply(1:input$num_footnotes, function(index) {
        # Dynamically generate an id for the text and url textInput fields
        text_input_id <- paste0("text", index)
        url_input_id <- paste0("url", index)

        # Dynamically create the label for the text and url textInput fields
        footnote_text_label <- paste0("Footnote ", index, " Text")
        footnote_url_label <- paste0("Optional: Footnote ", index, " URL")

        tags$div(
          class = "row m-1 p-1 border bg-light",
          tags$div(
            class = "col-md-6",
            # A text input for the footnote's text with a dynamically generated id
            shiny::textInput(ns(text_input_id), footnote_text_label, width = "100%", value = previous_text[[letters[index]]])
          ),
          tags$div(
            class = "col-md-6",
            # A text input for the footnote's URL with a dynamically generated id
            shiny::textInput(ns(url_input_id), footnote_url_label, width = "100%", value = previous_urls[[letters[index]]])
          )
        )
      })
    })

    output$preview <- renderUI({
      num_footnotes <- req(input$num_footnotes)

      # Generate a list of lists which contain text and optionally a URL
      footnote_list <- lapply(1:num_footnotes, function(index) {
        footnote_url <- input[[paste0("url", index)]]
        footnote_text <- input[[paste0("text", index)]]

        # If there is no text or url value, skip the iteration
        if (is.null(footnote_url) | is.null(footnote_text)) {
          return()
        }

        # If the URL is blank, only include the text value
        if (footnote_url == "") {
          list(text = footnote_text)
        }
        # Otherwise, include both the text and the url
        else {
          list(
            text = footnote_text,
            url = footnote_url
          )
        }
      })

      # Output the preview
      tagList(
        tags$h2("Preview"),
        add_page_foot(
          footnote_list
        )
      )
    })

    output$generated_code <- renderPrint({
      # Create an variable storing the opening part of the generated code
      generated_code <- "add_page_foot(\n  list(\n"

      # Loop through all inputs to create the text for the generated code
      for (index in 1:input$num_footnotes) {
        footnote_url <- input[[paste0("url", index)]]
        footnote_text <- input[[paste0("text", index)]]

        # If there is no text or url value, skip the iteration
        if (is.null(footnote_url) | is.null(footnote_text)) {
          return()
        }

        # Append the beginning of a new list item with just the text value
        generated_code <- paste0(generated_code, "    list(text = \"", footnote_text, "\"")

        # If there is a URL value for this iteration, append the url to the string
        if (footnote_url != "") {
          generated_code <- paste0(generated_code, ", url = \"", footnote_url, "\"")
        }

        # Close the list() with a parenthesis
        generated_code <- paste0(generated_code, ")")

        # If the current index is not the final index, append a comma to the text
        if (index < input$num_footnotes) {
          generated_code <- paste0(generated_code, ",")
        }

        # Append a new line to the end of the code
        generated_code <- paste0(generated_code, "\n")
      }

      # Append the closing parentheses to the generated code
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
# mod_page_foot_ui("page-foot")

## To be copied in the server
# mod_page_foot_server("page-foot")
