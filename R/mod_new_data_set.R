#' new_data_set UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_new_data_set_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("main"))
  )
}
    
#' new_data_set Server Functions
#'
#' @noRd 
mod_new_data_set_server <- function(id, section_selection){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$main <- renderUI({
      req(section_selection())

      if (section_selection() != "New Data Set") {
        return()
      }

      tagList(
      shiny::textInput(ns("sheet_name"), "Excel Sheet Name"),
      shiny::textInput(ns("variable_name"), "Optional: Variable Name"),
      shiny::verbatimTextOutput(ns("generated_code"))
      )
    })

    output$generated_code <- shiny::renderPrint({
      var_name <- gsub(" ", "_", tolower(ifelse(input$variable_name == "", input$sheet_name, input$variable_name)))

      cat(
        "# ", input$sheet_name, "\n",
        var_name, " <- readxl::read_xlsx(source, sheet = \"", input$sheet_name,"\", skip = 1)\n",
        "usethis::use_data(", var_name, ", overwrite = TRUE)",
        sep = ""
      )
    })
  })
}
    
## To be copied in the UI
# mod_new_data_set_ui("new-data")
    
## To be copied in the server
# mod_new_data_set_server("new-data")
