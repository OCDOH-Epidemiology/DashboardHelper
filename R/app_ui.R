#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::bootstrapPage(
      shinyjs::useShinyjs(),
      theme = bslib::bs_theme(version = 5),
      shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(disable = TRUE),
        shinydashboard::dashboardSidebar(disable = TRUE),
        shinydashboard::dashboardBody(
          tags$section(
            class = "p-4",
            shiny::fileInput("file_in", "Upload the JSON file here:", accept = c(".json")),
            shiny::actionButton("examine_json", "Execute"),
            shiny::uiOutput("head", class = "mt-3")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    tags$html(lang = "en"),
    tags$html(charset = "utf-8"),
    favicon(),

    # Link to Font Awesome
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),

    # Links to Google Fonts
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@100;400;600;700;900&family=Secular+One&display=swap"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DashboardHelper"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
