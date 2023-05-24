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
    bootstrapPage(
      theme = bslib::bs_theme(version = 5),
      tags$section(
        class = "p-4",
        shiny::selectInput("section_selection", "What code do you want to generate?", c("New Data Set", "Page Header", "Page Footer", "Section with One Indicator", "Section with Multiple Indicators")),
        mod_new_data_set_ui("new-data"),
        mod_page_head_ui("page-head"),
        mod_page_foot_ui("page-foot"),
        mod_section_with_one_indicator_ui("one-indicator"),
        mod_section_with_multiple_indicators_ui("multi-indicator")
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DashboardHelper"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
