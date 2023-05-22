#' format_id
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
format_id <- function(text) {
  gsub(" ", "-", tolower(text))
}
