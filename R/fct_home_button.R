#' make_home_button
#'
#' @description A function that creates a button that returns the user to the home page of the app
#' 
#' @details Uses Bootstrap 5.2.2 styling
#'
#' @return An HTML button that returns the user to the home page when pressed
#'
#' @noRd
make_home_button <- function() {
  tags$a(
    href = "#",
    class = "btn btn-secondary btn-sm",
    "Go Home"
  )
}
