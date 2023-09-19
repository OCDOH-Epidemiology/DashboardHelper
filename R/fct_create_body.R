#' Create the body of a report based on input data
#'
#' This function generates the body of a report based on the input data, including
#' sections for indicators, subindicators, and graphs.
#'
#' @param data_in A list of data containing indicators, subindicators, and graph data.
#' @return A list of HTML sections representing the report body.
#'
#' @noRd
create_body <- function(data_in, ns) {
  if (length(data_in) == 0) {
    return()
  }

  lapply(1:length(data_in), function(i) {
    background_color <- ifelse(i %% 2 == 0, "bg-water", "bg-white")

    tags$section(
      id = format_id(data_in[[i]]$indicator),
      class = paste0(background_color, " px-5 py-3"),
      tags$h3(class = "fw-bold", data_in[[i]]$indicator),

      # Generate description paragraphs
      lapply(1:length(data_in[[i]]$paragraphs), function(j) {
        tags$p(HTML(class = "", data_in[[i]]$paragraphs[[j]]))
      }),

      # Generate subindicator information if there is any
      if (!is.null(data_in[[i]]$subindicators)) {
        lapply(1:length(data_in[[i]]$subindicators), function(j) {
          tags$div(
            if (data_in[[i]]$subindicators[[j]]$indicator != "") {
              tags$h5(class = "fw-bold", data_in[[i]]$subindicators[[j]]$indicator)
            },
            lapply(1:length(data_in[[i]]$subindicators[[j]]$paragraphs), function(k) {
              tags$p(HTML(class = "", data_in[[i]]$subindicators[[j]]$paragraphs[[k]]))
            })
          )
        })
      },

      # Generate a header element for the main finding if there is one
      if (!is.null(data_in[[i]]$main_finding)) {
        tags$h3(class = "text-center fw-bold mt-3", data_in[[i]]$main_finding)
      },

      # Generate the area for the graphs in the current section
      tags$div(
        class = "row mt-1 g-4 justify-content-center",

        # Generate each graph in its own column with its finding, title, and footnote
        lapply(1:length(data_in[[i]]$graph_data), function(j) {
          tags$div(
            class = "col-lg-6",
            tags$div(
              class = "w-100 h-100 d-flex flex-column justify-content-between",
              # Generate a header element for the finding of the graph if there is one
              if (!is.null(data_in[[i]]$graph_data[[j]]$finding)) {
                tags$h3(class = "text-center fs-3 fw-bold", data_in[[i]]$graph_data[[j]]$finding)
              },
              tags$h5(class = "text-center mt-4", data_in[[i]]$graph_data[[j]]$title),
              shinipsum::random_ggplotly(type = "bar"),
              #mod_add_graph_ui(ns(format_id(data_in[[i]]$graph_data[[j]]$id))),
              tags$div(style = "font-size: .75rem;", class = "px-5", HTML(data_in[[i]]$graph_data[[j]]$footnote))
            )
          )
        })
      )
    )
  })
}
