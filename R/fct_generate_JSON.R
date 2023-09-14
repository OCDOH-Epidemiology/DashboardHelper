#' generate_list_from_inputs
#' 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
generate_list_from_inputs <- function(input) {
      req(input$file_in)

      # Create the JSON file to download
      exportFile <- list()

      # Check to make sure the head indicator has a value, send an alert if it doesn't
      if (input[["head-indicator"]] == "") shinyjs::alert("Header indicator input does not have a value.")
      exportFile$head$indicator <- input[["head-indicator"]]

      # Add each header paragraph to the exportFile
      exportFile$head$paragraphs <- lapply(1:input[["head-number-of-paragraphs"]], function(i) {
        # Check that all textInputs have a value, send an alert if it doesn't
        if (input[[paste0("head-paragraph-", i)]] == "") shinyjs::alert(paste0("Header paragraph input ", i, " does not have a value."))

        # Add the value of the textInput to exportFile$head$paragraphs
        input[[paste0("head-paragraph-", i)]]
      })

      # Add each header button to the exportFile
      exportFile$head$buttons <- lapply(1:input[["head-number-of-buttons"]], function(i) {
        # Check that all textInputs have a value, send an alert if it doesn't
        if (input[[paste0("head-button-", i)]] == "") shinyjs::alert(paste0("Header button input ", i, " does not have a value."))

        # Add the value of the textInput to exportFile$head$buttons
        input[[paste0("head-button-", i)]]
      })

      # Add all the body data to the exportFile
      exportFile$body <- lapply(1:input[["number-of-sections"]], function(i) {
        body_list <- list()
        body_list$indicator <- input[[paste0("section-indicator-", i)]]

        # If the main finding is not blank, add it to the list
        if (input[[paste0("section-main-finding-", i)]] != "") {
          body_list$main_finding <- input[[paste0("section-main-finding-", i)]]
        }

        # If there is one or more paragraphs, add them to the list
        if (input[[paste0("section-number-of-paragraphs", i)]] > 0) {
          body_list$paragraphs <- lapply(1:input[[paste0("section-number-of-paragraphs", i)]], function(j) {
            input[[paste0("section-paragraph-", i, j)]]
          })
        }

        # If there is one or more subindicators, add them to the list
        if (input[[paste0("section-number-of-subindicators", i)]] > 0) {
          body_list$subindicators <- lapply(1:input[[paste0("section-number-of-subindicators", i)]], function(j) {
            list(
              indicator = input[[paste0("section-subindicator-", i, j)]],
              paragraphs = lapply(1:input[[paste0("section-number-of-subindicator-paragraphs-", i, j)]], function(k) {
                input[[paste0("section-subindicator-paragraph-", i, j, k)]]
              })
            )
          })
        }

        # Graph stuff
        body_list$graph_data <- lapply(1:input[[paste0("section-number-of-graphs-", i)]], function(j) {
          graph_list <- list()

          graph_list$id <- input[[paste0("graph-id-", i, j)]]
          graph_list$title <- input[[paste0("graph-title-", i, j)]]
          graph_list$footnote <- input[[paste0("graph-footnote-", i, j)]]
          graph_list$data_source <- input[[paste0("graph-data-source-", i, j)]]
          graph_list$y_title <- input[[paste0("graph-y-title-", i, j)]]
          graph_list$y_format <- input[[paste0("graph-y-format-", i, j)]]
          graph_list$hover_format <- input[[paste0("graph-hover-format-", i, j)]]

          if (input[[paste0("graph-finding-", i, j)]] != "") {
            graph_list$finding <- json_data$body[[i]]$graph_data[[j]]$finding
          }

          return(graph_list)
        })

        return(body_list)
      })

      # Add all the footer data to the exportFile
      if (input[["number-of-footnotes"]] > 0) {
        exportFile$foot <- lapply(1:input[["number-of-footnotes"]], function(i) {
          foot_list <- list()
          foot_list$text <- input[[paste0("footnote-text-", i)]]

          if (input[[paste0("footnote-url-", i)]] != "") {
            foot_list$url <- input[[paste0("footnote-url-", i)]]
          }

          return(foot_list)
        })
      }

      return(exportFile)
    }