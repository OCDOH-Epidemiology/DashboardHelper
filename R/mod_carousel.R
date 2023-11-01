#' carousel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_carousel_ui <- function(id, section_data, carousel_style = "light") {
  ns <- NS(id)

  number_of_graphs <- length(section_data$carousel_data)

  tagList(
    tags$div(
      id = paste0(id, "-carousel"), class = paste0("carousel slide carousel-fade h-100", if (carousel_style == "dark") {
        " carousel-dark"
      }), `data-bs-ride` = "false", `data-interval` = "4000",
      tags$div(
        class = "carousel-indicators",
        lapply(1:number_of_graphs, function(i) {
          if (i == 1) {
            tags$button(`data-bs-target` = paste0("#", id, "-carousel"), `data-bs-slide-to` = "0", class = "active", `aria-current` = "true", `aria-label` = paste("Slide", i))
          } else {
            tags$button(`data-bs-target` = paste0("#", id, "-carousel"), `data-bs-slide-to` = i - 1, `aria-label` = paste("Slide", i))
          }
        })
      ),
      tags$div(
        class = "carousel-inner", style = "padding: 3rem 10%;",
        lapply(1:number_of_graphs, function(i) {
          div_class <- paste("carousel-item", if (i == 1) {
            "active"
          }, ifelse(carousel_style == "dark", "bg-water", "bg-teal"))

          tags$div(
            class = div_class,
            tags$h3(class = "text-center fw-bold", section_data$carousel_data[[i]]$data_observation),
            tags$h5(class = "text-center", section_data$carousel_data[[i]]$graph_title),
            plotly::plotlyOutput(ns(paste0("graph", i)), width = "100%"),
            shiny::actionButton(ns(paste0("modal", i)), "Learn more", class = "mt-1")
          )
        })
      ),
      tags$button(
        class = "carousel-control-prev h-50 my-auto", `data-bs-target` = paste0("#", id, "-carousel"), `data-bs-slide` = "prev",
        tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
        tags$span(class = "visually-hidden", "Previous")
      ),
      tags$button(
        class = "carousel-control-next h-50 my-auto", `data-bs-target` = paste0("#", id, "-carousel"), `data-bs-slide` = "next",
        tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
        tags$span(class = "visually-hidden", "Next")
      )
    )
  )
}

#' carousel Server Functions
#'
#' @noRd
mod_carousel_server <- function(id, section_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(1:length(section_data$carousel_data), function(i) {
      current_graph <- section_data$carousel_data[[i]]

      # Depending on the data format type of the current graph,
      # assign the appropriate data source to `data_source`
      data_source <- switch(current_graph$data_format_type,
        "rate" = sample_rate_data, # For "rate", use sample_rate_data
        "percent" = sample_percent_data, # For "percent", use sample_percent_data
        "currency" = sample_currency_data # For "currency", use sample_currency_data
      )

      # Assign a format string to `y_format` based on the data format type of the current graph.
      # This format string will be used for the y-axis labels
      y_format <- switch(current_graph$data_format_type,
        "rate" = ",0", # For "rate", format with no decimal places
        "percent" = ".0%", # For "percent", format as percentage with no decimal places
        "currency" = "$," # For "currency", format with a dollar sign and no decimal places
      )

      # Assign a format string to `hover_format` based on the data format type of the current graph.
      # This format string will be used for the tooltip/hover information
      hover_format <- switch(current_graph$data_format_type,
        "rate" = ",.1f", # For "rate", format with one decimal place
        "percent" = ".1%", # For "percent", format as percentage with one decimal place
        "currency" = "$,.0f" # For "currency", format with a dollar sign and no decimal places
      )

      # Retrieve the number of columns from the data source
      num_columns <- ncol(data_source)

      # Assign the y-title and y-axis title from the current graph
      y_title <- current_graph$y_title
      y_axis_title <- current_graph$y_axis_title

      # Define a color palette for the graph
      fig_colors <- c("#007b85", "#61c2ee", "#00a79e", "#67a142")

      # If there are more than 4 columns, repeat the colors to cover all columns.
      # The ceiling function ensures that even if there's a partial set needed (e.g., 5 columns),
      # it would still repeat the whole set, so all columns get a color.
      fig_colors <- rep(fig_colors, ceiling(num_columns / 4))

      output[[paste0("graph", i)]] <- plotly::renderPlotly({
        fig <- plotly::plot_ly()

        # Dynamically create bars for the graph based on the number of columns
        for (i in 2:num_columns) {
          # formatted_text <- format_text_values(data_source[[i]], hover_format)

          fig <- fig %>% plotly::add_trace(
            data = data_source,
            x = ~ data_source[[1]],
            y = ifelse(is.na(data_source[[i]]), 0, data_source[[i]]),
            name = colnames(data_source)[i],
            type = "bar",
            marker = list(color = fig_colors[i - 1]),
            text = ifelse(is.na(data_source[[i]]), "S", data_source[[i]]),
            textposition = "outside",
            texttemplate = ifelse(is.na(data_source[[i]]), "%{text}", paste0("%{y:", hover_format, "}")),
            hovertemplate = paste0(
              "<b>", colnames(data_source)[i], "<br>%{x}</b><br>",
              paste0("%{text:", hover_format, "}"), "<extra></extra>"
            )
          )
        }

        # Set the layout of the graph
        fig <- fig %>% plotly::layout(
          xaxis = list(title = "", categoryorder = "trace", fixedrange = TRUE),
          yaxis = list(title = y_axis_title, tickformat = y_format, hoverformat = hover_format, fixedrange = TRUE),
          legend = list(orientation = "h", x = 0.5, xanchor = "center"),
          uniformtext = list(minsize = 8, mode = "hide"),
          dragmode = FALSE
        )

        # Render the graph
        fig
      })

      observeEvent(input[[paste0("modal", i)]], {
        shiny::showModal(
          shiny::modalDialog(
            title = HTML(paste("More about", tags$span(class = "text-decoration-underline fw-bold", current_graph$data_name))),
            tags$div(
              tags$span(class = "fw-bold", "Sources:"),
              tags$p(class = "fs-6 mb-1", current_graph$data_source)
            ),
            tags$hr(class = "my-2"),
            tags$div(
              DT::renderDataTable({
                unformatted_table <- data_source %>%
                  DT::datatable(
                    options = list(
                      pageLength = 25,
                      scrollX = TRUE,
                      ordering = FALSE,
                      paging = FALSE,
                      searching = FALSE,
                      lengthChange = FALSE,
                      info = FALSE,
                      columnDefs =
                        list(list(
                          className = "dt-center",
                          targets = 1:(num_columns - 1)
                        )),
                      rowCallback = DT::JS("replaceNAWithS")
                    ),
                    rownames = FALSE
                  )

                formatted_table <- switch(current_graph$data_format_type,
                  "rate" = unformatted_table %>% DT::formatRound(2:num_columns, 1),
                  "percent" = unformatted_table %>% DT::formatPercentage(2:num_columns, 1),
                  "currency" = unformatted_table %>% DT::formatCurrency(2:num_columns, 0)
                )

                formatted_table
              })
            ),
            tags$hr(),
            tags$div(
              tags$span(class = "fw-bold", "Notes:"),
              tags$p(class = "fs-6 mb-1", HTML(current_graph$data_note))
            ),
            footer = tagList(
              tags$span(HTML(paste(tags$span(class = "fw-bold", "Last Updated:"), current_graph$last_updated))),
              shiny::modalButton("Dismiss")
            )
          )
        )
      },
      ignoreInit = TRUE)

      outputOptions(output, paste0("graph", i), suspendWhenHidden = FALSE)
    })
  })
}

## To be copied in the UI
# mod_carousel_ui("carousel_1")

## To be copied in the server
# mod_carousel_server("carousel_1")
