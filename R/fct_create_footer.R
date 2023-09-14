#' Create the footer section of the page
#'
#' This function generates the footer section of a webpage based on the input
#' footnotes. Each footnote is displayed in a formatted manner, including
#' a superscript number and a link to external sources if available.
#'
#' @param data_in A list containing information about the webpage's footer, including:
#'   \itemize{
#'     \item{text: The text of the footer (character).}
#'     \item{url: An optional URL link to a datasource for the text (character vector).}
#'   }
#' @return An HTML section representing the webpage's footer.
#' 
#' @noRd 
create_footer <- function(data_in) {
  # Quit early if no footnotes are provided
  if (length(data_in) == 0) {
    return()
  }

  tags$section(
    class = "bg-outer-space px-5 py-3",
    lapply(1:length(data_in), function(x) {
      current_footnote <- data_in[[x]]

      tags$p(
        class = "m-0 px-0 py-1",
        tags$sup(x),
        if (length(current_footnote) == 1) {
          current_footnote$text
        } else {
          tags$a(
            class = "footnote-link",
            target = "_blank",
            href = current_footnote$url,
            current_footnote$text
          )
        }
      )
    })
  )
}
