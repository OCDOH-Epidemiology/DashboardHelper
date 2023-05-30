#' add_head
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
add_head <- function(indicator, description = list(), sections = list()) {
  tags$section(
    class = "position-relative px-5 py-3",
    style = "background-color: rgb(215, 236, 250); color: rgb(64, 81, 78);",
    home_button(),
    tags$h1(indicator),
    lapply(description, function(x) {
      tags$p(HTML(x))
    }),
    tags$div(
      class = "d-flex justify-content-center",
      lapply(sections, function(x) {
        section_button(x)
      })
    )
  )
}
