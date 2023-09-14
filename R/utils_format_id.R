#' format_id
#'
#' @description A utils function which removes all non-alphabetical characters from a string to form a valid ID
#'
#' @param text - A string of text that can contain any characters
#' 
#' @return A string of text containing only lowercase letters and hyphens
#'
#' @noRd
format_id <- function(text) {
  tolower(stringr::str_replace_all(stringr::str_squish(stringr::str_replace_all(text, "[^[:alnum:]]", " ")), " ", "-"))
}
