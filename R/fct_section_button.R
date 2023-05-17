#' section_button
#'
#' @description A utils function that creates a button that scrolls the page to
#'   a section based on the section ID
#'
#' @details Uses Bootstrap 5.2.2 styling
#'
#' @param text A string variable that is used to make the text of the button
#'   and href ID by replacing " " with "-"
#'
#' @return An HTML anchor tag linked to a section on the page
#'
#' @noRd
section_button <- function(text) {
  id <- gsub(" ", "-", tolower(text))

  return(a(class = "btn btn-dark mx-3", href = paste0("#", id), text))
}
