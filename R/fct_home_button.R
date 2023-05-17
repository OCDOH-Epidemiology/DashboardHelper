#' home_button
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
home_button <- function() {
  tags$a(
    href = "#",
    class = "btn btn-secondary btn-sm",
    "Go Home"
  )
}
