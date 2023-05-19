#' add_section_with_multiple_indicators
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
add_section_with_multiple_indicators <- function(ns, indicator, sub_indicators = list(), bg_color = "bg-white", main_finding = "", ...) {
    tags$section(
        id = gsub(" ", "-", tolower(indicator)),
        class = paste0(bg_color, " px-5 py-3"),
        tags$h3(class = "fw-bold", indicator),
        lapply(sub_indicators, function(x) {
            tags$div(
                tags$h5(class = "fw-bold", x$indicator),
                HTML(class = "", x$description)
            )
        }),
        tags$h3(class = "text-center fw-bold mt-4", main_finding),
        tags$div(
            class = "row mt-1 g-4 justify-content-center",
            lapply(list(...), function(x) {
                tags$div(
                    class = "col-lg-6",
                    tags$div(
                        class = "w-100 h-100 d-flex flex-column justify-content-between",
                        tags$h3(class = "text-center fw-bold", x$finding),
                        tags$h5(class = "text-center mt-4", x$title),
                        mod_add_graph_ui(ns(x$id)),
                        tags$div(style = "font-size: .75rem;", class = "px-5", HTML(x$footnote))
                    )
                )
            })
        )
    )
}