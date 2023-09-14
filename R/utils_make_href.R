#' make_href
#'
#' @description A utils function that creates a link based on the page endpoint
#'
#' @param endpoint
#'
#' @return An HTML anchor tag linked to a section on the page
#'
#' @noRd
make_href <- function(endpoint) {
  # Get the baseurl of the webpage
  baseurl <- getOption("baseurl")

  # If baseurl != "", append a "/" to the start of the baseurl and endpoint to the end
  # This creates the full link needed to be able to switch to the page
  if (baseurl != "") {
    return(paste0("/", baseurl, endpoint, sep = ""))
  }
  # Otherwise, the endpoint is the entire link
  else {
    return(endpoint)
  }
}
