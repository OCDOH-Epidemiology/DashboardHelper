#' add_page_foot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
add_page_foot <- function(footnotes = list()) {
  # Quit early if no footnotes sent in
  if (length(footnotes) == 0) {
    return()
  }

  footnote_count <- 0

  tags$section(
    class = "bg-outer-space px-5 py-3",
    lapply(footnotes, function(x) {
      footnote_count <<- footnote_count + 1
      tags$p(
        class = "m-0 px-0 py-1",
        tags$sup(footnote_count),
        if (length(x) == 1) {
          x$text
        } else if (length(x) == 2) {
          tags$a(
            class = "footnote-link",
            target = "_blank",
            href = x$url,
            x$text
          )
        }

      )
    })
  )
}
