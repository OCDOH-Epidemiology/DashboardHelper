#' add_section_with_one_indicator
#'
#' @description A function which creates a section. The section contains one indicator with a description and a dynamic number of graphs.
#'
#' @return An HTML section containing descriptions of indicators, insights, and graphs.
#'
#' @param ns: The namespace variable from the parent module
#' @param indicator: The name of the indicator for the section
#' @param description: The text description of the indicator
#' @param bg_color: The background color to be used for the section
#' @param main_finding: A text value that will be displayed with the full width of the section
#' @param ...: A dynamic number of list variables used to create the different graphs and associated text
#'            Each list must contain an id, and can contain a finding, title, and footnote
#'
#' @noRd
add_section_with_one_indicator <- function(ns, indicator, description, bg_color = "bg-white", main_finding = "", graphs) {
  tags$section(
    id = format_id(indicator),
    class = paste0(bg_color, " px-5 py-3 border"),
    tags$h3(class = "fw-bold", indicator),
    lapply(description, function(x) {
      tags$p(HTML(class = "", x))
    }),
    tags$h3(class = "text-center fw-bold mt-3", main_finding),
    tags$div(
      class = "row mt-1 g-4 justify-content-center",
      lapply(graphs, function(x) {
        tags$div(
          class = "col-lg-6",
          tags$div(
            class = "w-100 h-100 d-flex flex-column justify-content-between",
            tags$h3(class = "text-center fs-3 fw-bold", HTML(x$finding)),
            tags$h5(class = "text-center mt-4", HTML(x$title)),
            shinipsum::random_ggplotly("bar"),
            tags$div(style = "font-size: .75rem;", class = "px-5", HTML(x$footnote))
          )
        )
      })
    )
  )
}