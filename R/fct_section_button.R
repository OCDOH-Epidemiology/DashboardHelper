#' make_section_button
#'
#' @description A function that creates a button that scrolls the page to a section based on the section ID
#' 
#' @details Uses Bootstrap 5.2.2 styling
#'
#' @param text: A string variable that is used to make the text of the button and href ID
#' 
#' @return An HTML anchor tag linked to a section on the page
#'
#' @noRd
make_section_button <- function(text) {
  href_id <- gsub(" ", "-", tolower(text))

  return(a(class = "btn btn-dark mx-3", href = paste0("#", href_id), text))
}
