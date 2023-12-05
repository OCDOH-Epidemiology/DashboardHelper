#' Create the body of a report based on input data
#'
#' This function generates the body of a report based on the input data, including
#' sections for indicators, subindicators, and graphs.
#'
#' @param data_in A list of data containing indicators, subindicators, and graph data.
#' @return A list of HTML sections representing the report body.
#'
#' @noRd
create_body <- function(body_data, preview_num) {
  if (length(body_data) == 0) {
    return()
  }

  lapply(1:length(body_data), function(i) {
      # Pull the variables for creating the section
      current_section <- body_data[[i]]
      indicator <- current_section$indicator
      number_of_paragraphs <- length(current_section$paragraphs)
      number_of_subindicators <- length(current_section$subindicators)
      number_of_graphs <- length(current_section$carousel_data)

      indicator_description_area <- tagList(
        tags$h1(indicator),
        if (number_of_paragraphs > 0) {
          lapply(1:number_of_paragraphs, function(j) {
            tags$p(HTML(current_section$paragraphs[[j]]))
          })
        },
        if (number_of_subindicators > 0) {
          lapply(1:number_of_subindicators, function(j) {
            # Store the current subindicator
            current_subindicator <- current_section$subindicators[[j]]

            # Count how many paragraphs there are for the current subindicator
            number_of_sub_paragraphs <- length(current_subindicator$paragraphs)

            tagList(
              tags$h3(current_subindicator$indicator),
              if (number_of_sub_paragraphs > 0) {
                lapply(1:number_of_sub_paragraphs, function(k) {
                  tags$p(HTML(current_subindicator$paragraphs[[k]]))
                })
              }
            )
          })
        }
      )

      tags$section(
        id = tolower(gsub(" ", "-", indicator)),
        class = paste("row py-3 m-0", ifelse(i %% 2 == 0, "bg-water", "bg-teal")),
        if (i %% 2 == 0) {
          tagList(
            div(class = "col-xxl-6 col-xl-5 px-5 description-area order-xl-1 align-self-center", indicator_description_area),
            div(class = "col-xxl-6 col-xl-7 order-xl-2", mod_carousel_ui(paste0(tolower(gsub(" ", "-", indicator)), preview_num), current_section, "dark"))
          )
        } else {
          tagList(
            div(class = "col-xxl-6 col-xl-5 px-5 description-area order-xl-2 align-self-center", indicator_description_area),
            div(class = "col-xxl-6 col-xl-7 order-xl-1", mod_carousel_ui(paste0(tolower(gsub(" ", "-", indicator)), preview_num), current_section, "light"))
          )
        }
      )
    })
}