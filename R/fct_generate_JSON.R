#' generate_list_from_inputs
#'
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
generate_list_from_inputs <- function(input) {
  # Create the JSON file to download
  exportFile <- list()

  exportFile$head <- generate_head_list(input)
  exportFile$body <- generate_body_list(input)
  exportFile$foot <- generate_foot_list(input)

  return(exportFile)
}

generate_head_list <- function(input) {
  return_list <- list()
  return_list$indicator <- input$page_indicator

  # Add each header paragraph to the exportFile
  return_list$paragraphs <- lapply(1:input$header_num_paragraphs, function(i) {
    input[[paste0("header_paragraph", i)]]
  })

  # Add each header button to the exportFile
  return_list$buttons <- lapply(1:input$header_num_buttons, function(i) {
    input[[paste0("header_button", i)]]
  })

  return(return_list)
}

generate_body_list <- function(input) {
  lapply(1:input$num_sections, function(i) {
    body_list <- list()
    body_list$indicator <- input[[paste0("section_indicator", i)]]

    # If there is one or more paragraphs, add them to the list
    if (input[[paste0("section_num_paragraphs", i)]] > 0) {
      body_list$paragraphs <- lapply(1:input[[paste0("section_num_paragraphs", i)]], function(j) {
        input[[paste0("section_paragraphs", i, j)]]
      })
    }

    # If there is one or more subindicators, add them to the list
    if (input[[paste0("num_subindicators", i)]] > 0) {
      body_list$subindicators <- lapply(1:input[[paste0("num_subindicators", i)]], function(j) {
        list(
          indicator = input[[paste0("subindicator", i, j)]],
          paragraphs = lapply(1:input[[paste0("subindicator_num_paragraph", i, j)]], function(k) {
            input[[paste0("subindicator_paragraph", i, j, k)]]
          })
        )
      })
    }

    # Graph stuff
    body_list$carousel_data <- lapply(1:input[[paste0("section_num_graphs", i)]], function(j) {
      list(
        "data_format_type" = input[[paste0("data_format_type", i, j)]],
        "data_name" = input[[paste0("data_name", i, j)]],
        "data_note" = input[[paste0("data_note", i, j)]],
        "data_observation" = input[[paste0("data_observation", i, j)]],
        "data_source" = input[[paste0("data_source", i, j)]],
        "excel_sheet_name" = tolower(gsub(" ", "_", input[[paste0("excel_sheet_name", i, j)]])),
        "graph_title" = input[[paste0("graph_title", i, j)]],
        "last_updated" = format(input[[paste0("last_updated", i, j)]], "%B %d, %Y"),
        "y_axis_title" = input[[paste0("y_axis_title", i, j)]]
      )
    })

    return(body_list)
  })
}

generate_foot_list <- function(input) {
  if (input$num_footnotes > 0) {
    lapply(1:input$num_footnotes, function(i) {
      foot_list <- list()
      foot_list$text <- input[[paste0("footnote_text", i)]]

      if (input[[paste0("footnote_link", i)]] != "") {
        foot_list$url <- input[[paste0("footnote_link", i)]]
      }

      return(foot_list)
    })
  }
}
