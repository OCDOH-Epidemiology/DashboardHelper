#' add_body
#'
#' @description A function which creates a section. The section contains multiple indicators, each with a description and a dynamic number of graphs.
#'
#' @return An HTML section containing descriptions of indicators, insights, and graphs.
#'
#' @param ns: The namespace variable from the parent module
#' @param indicator: The name of the main indicator for the section
#' @param sub_indicators: A list of lists in which the title and description of the sub-indicators are defined
#' @param bg_color: The background color to be used for the section
#' @param main_finding: A text value that will be displayed with the full width of the section
#' @param ...: A dynamic number of list variables used to create the different graphs and associated text
#'            Each list must contain an id, and can contain a finding, title, and footnote
#'
#' @noRd
add_body <- function(ns, main_indicator, main_finding = "", sub_indicators = list(), bg_color = "bg-white", graphs) {
  tags$section(
    id = format_id(main_indicator),
    class = paste0(bg_color, " px-5 py-3"),
    tags$h3(class = "fw-bold", main_indicator),
    lapply(sub_indicators, function(sub_indicator) {
      tags$div(
        tags$h5(class = "fw-bold", sub_indicator$indicator),
        lapply(sub_indicator$description, function(description) {
          tags$p(HTML(class = "", description))
        })
      )
    }),
    tags$h3(class = "text-center fw-bold mt-3", main_finding),
    tags$div(
      class = "row mt-1 g-4 justify-content-center",
      lapply(graphs, function(graph_input) {
        tags$div(
          class = "col-lg-6",
          tags$div(
            class = "w-100 h-100 d-flex flex-column justify-content-between",
            tags$h3(class = "text-center fs-3 fw-bold", graph_input$finding),
            tags$h5(class = "text-center mt-4", graph_input$title),
            plotly::plotlyOutput(ns(format_id(graph_input$id))),
            tags$div(style = "font-size: .75rem;", class = "px-5", HTML(graph_input$footnote))
          )
        )
      })
    )
  )
}
