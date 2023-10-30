#' reset_inputs
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
reset_inputs <- function(session, constants) {
  reset_head(session, constants)
  reset_body(session, constants)
  reset_foot(session, constants)
}

reset_head <- function(session, constants) {
  shiny::updateTextInput(session, "page_indicator", value = "")
  shiny::updateSliderInput(session, "head_num_paragraphs", value = 1)
  shiny::updateSliderInput(session, "header_num_buttons", value = 1)
  for (i in 1:constants$MAX_HEADER_PARAGRAPHS) {
    shiny::updateTextAreaInput(session, paste0("header_paragraph", i), value = "")
  }
  for (i in 1:constants$MAX_HEADER_BUTTONS) {
    shiny::updateTextInput(session, paste0("header_button", i), value = "")
  }
}

reset_body <- function(session, constants) {
  # Reset the slider for the number of sections in the page
  shiny::updateSliderInput(session, "num_sections", value = 1)

  # Loop through each section (visible or not) and reset the values
  for (i in 1:constants$MAX_BODY_SECTIONS) {
    # Reset the section indicator
    shiny::updateTextInput(session, paste0("section_indicator", i), value = "")

    # Reset the slider determining the number of paragraphs for the section
    shiny::updateSliderInput(session, paste0("section_num_paragraphs", i), value = 0)
    # Reset the text area inputs for the paragraphs
    for (j in 1:constants$MAX_BODY_SECTION_PARAGRAPHS) {
      shiny::updateTextAreaInput(session, paste0("section_paragraphs", i, j), value = "")
    }

    # Reset the slider determining the number of subindicators for the section
    shiny::updateSliderInput(session, paste0("num_subindicators", i), value = 0)
    # Loop through each subindicator input section and reset all values
    for (j in 1:constants$MAX_BODY_SECTION_SUBINDICATORS) {
      shiny::updateTextInput(session, paste0("subindicator", i, j), value = "")
      shiny::updateSliderInput(session, paste0("subindicator_num_paragraph", i, j), value = 1)
      for (k in 1:constants$MAX_BODY_SECTION_SUBINDICATOR_PARAGRAPHS) {
        shiny::updateTextAreaInput(session, paste0("subindicator_paragraph", i, j, k), value = "")
      }
    }

    # Reset the slider determining the number of graphs for the section
    shiny::updateSliderInput(session, paste0("section_num_graphs", i), value = 1)
    # Loop through each graph input section and reset all values
    for (j in 1:constants$MAX_BODY_SECTION_GRAPHS) {
      shiny::updateTextInput(session, paste0("data_name", i, j), value = "")
      shiny::updateTextInput(session, paste0("excel_sheet_name", i, j), value = "")
      shiny::updateTextAreaInput(session, paste0("data_observation", i, j), value = "")
      shiny::updateTextAreaInput(session, paste0("graph_title", i, j), value = "")
      shiny::updateTextAreaInput(session, paste0("data_source", i, j), value = "")
      shiny::updateTextAreaInput(session, paste0("data_note", i, j), value = "")
      shiny::updateTextInput(session, paste0("y_axis_title", i, j), value = "")
      shiny::updateSelectizeInput(session, paste0("data_format_type", i, j), selected = "rate")
      shiny::updateDateInput(session, paste0("last_updated", i, j), value = Sys.Date())
    }
  }
}

reset_foot <- function(session, constants) {
  shiny::updateSliderInput(session, "num_footnotes", value = 0)
  for (i in 1:constants$MAX_FOOTER_FOOTNOTES) {
    shiny::updateTextInput(session, paste0("footnote_text", i), value = "")
    shiny::updateTextInput(session, paste0("footnote_link", i), value = "")
  }
}
