#' Create the header section of a webpage based on input data
#'
#' This function generates the header section of a webpage based on the input data.
#' It includes the main indicator, paragraphs, a home button, and section navigation buttons.
#'
#' @param data_in A list containing information about the webpage's header, including:
#'   \itemize{
#'     \item{indicator: The main indicator text (character).}
#'     \item{paragraphs: A list of description paragraphs (character vector).}
#'     \item{buttons: A list of section titles for navigation (character vector).}
#'   }
#' @return An HTML section representing the webpage's header.
#'
#' @noRd
create_header <- function(data_in) {
  tags$section(
    class = "position-relative px-5 py-3 bg-water",
    make_home_button(),
    tags$h1(data_in$indicator),
    lapply(data_in$paragraphs, function(x) {
      tags$p(x)
    }),
    tags$div(
      class = "d-flex justify-content-center",
      lapply(data_in$buttons, function(x) {
        make_section_button(x)
      })
    )
  )
}
