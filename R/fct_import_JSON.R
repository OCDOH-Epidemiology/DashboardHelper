#' import_JSON
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
import_JSON <- function(session, json_data) {
  # Set head inputs based on json_data
  shiny::updateTextInput(session, "page_indicator", value = json_data$head$indicator)
  shiny::updateSliderInput(session, "header_num_paragraphs", value = length(json_data$head$paragraphs))
  shiny::updateSliderInput(session, "header_num_buttons", value = length(json_data$head$buttons))
  for (i in 1:length(json_data$head$paragraphs)) {
    shiny::updateTextAreaInput(session, paste0("header_paragraph", i), value = json_data$head$paragraphs[[i]])
  }
  for (i in 1:length(json_data$head$buttons)) {
    shiny::updateTextInput(session, paste0("header_button", i), value = json_data$head$buttons[[i]])
  }

  # Set body inputs based on json_data
  shiny::updateSliderInput(session, "num_sections", value = length(json_data$body))
  for (i in 1:length(json_data$body)) {
    shiny::updateTextInput(session, paste0("section_indicator", i), value = json_data$body[[i]]$indicator)
    
    shiny::updateSliderInput(session, paste0("section_num_paragraphs", i), value = length(json_data$body[[i]]$paragraphs))
    # Check if there are paragraphs, if there are, fill the subindicator boxes
    if (length(json_data$body[[i]]$paragraphs) > 0) {
      for (j in 1:length(json_data$body[[i]]$paragraphs)) {
        shiny::updateTextAreaInput(session, paste0("section_paragraphs", i, j), value = json_data$body[[i]]$paragraphs[[j]])
      }
    }

    shiny::updateSliderInput(session, paste0("num_subindicators", i), value = length(json_data$body[[i]]$subindicators))
    # Check if there are subindicators, if there are, fill the subindicator boxes
    if (length(json_data$body[[i]]$subindicators) > 0) {
      for (j in 1:length(json_data$body[[i]]$subindicators)) {
        shiny::updateTextInput(session, paste0("subindicator", i, j), value = json_data$body[[i]]$subindicators[[j]]$indicator)
        shiny::updateSliderInput(session, paste0("subindicator_num_paragraph", i, j), value = length(json_data$body[[i]]$subindicators[[j]]$paragraphs))
        for (k in 1:length(json_data$body[[i]]$subindicators[[j]]$paragraphs)) {
          shiny::updateTextAreaInput(session, paste0("subindicator_paragraph", i, j, k), value = json_data$body[[i]]$subindicators[[j]]$paragraphs[[k]])
        }
      }
    }

    # Set the values for the graph inputs
    shiny::updateSliderInput(session, paste0("section_num_graphs", i), value = length(json_data$body[[i]]$carousel_data))
    for (j in 1:length(json_data$body[[i]]$carousel_data)) {
      current_graph_data <- json_data$body[[i]]$carousel_data[[j]]

      shiny::updateTextInput(session, paste0("data_name", i, j), value = current_graph_data$data_name)
      shiny::updateTextInput(session, paste0("excel_sheet_name", i, j), value = gsub("_", " ", current_graph_data$excel_sheet_name))
      shiny::updateTextInput(session, paste0("data_observation", i, j), value = ifelse(is.null(current_graph_data$data_observation), "", current_graph_data$data_observation))
      shiny::updateTextInput(session, paste0("graph_title", i, j), value = current_graph_data$graph_title)
      shiny::updateTextInput(session, paste0("data_source", i, j), value = current_graph_data$data_source)
      shiny::updateTextInput(session, paste0("data_note", i, j), value = current_graph_data$data_note)
      shiny::updateTextInput(session, paste0("y_axis_title", i, j), value = current_graph_data$y_axis_title)
      shiny::updateSelectizeInput(session, paste0("data_format_type", i, j), selected = current_graph_data$data_format_type)
      shiny::updateDateInput(session, paste0("last_updated", i, j), value = as.Date(current_graph_data$last_updated, format = "%B %d, %Y"))
    }
  }

  # Set foot inputs based on json_data
  shiny::updateSliderInput(session, "num_footnotes", value = length(json_data$foot))
  if (length(json_data$foot) > 0) {
    for (i in 1:length(json_data$foot)) {
      shiny::updateTextInput(session, paste0("footnote_text", i), value = json_data$foot[[i]]$text)
      shiny::updateTextInput(session, paste0("footnote_link", i), value = ifelse(is.null(json_data$foot[[i]]$url), "", json_data$foot[[i]]$url))
    }
  }
}
