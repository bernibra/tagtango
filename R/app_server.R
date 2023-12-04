options(shiny.maxRequestSize = 100*1024^2)

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # defining reactive values
  values <- reactiveValues()

  # Landing page
  output$content <- renderUI(mod_input_data_ui("input_data_1"))

  # Landing page logic
  data <- mod_input_data_server("input_data_1")
  shinyjs::disable("load")

  observeEvent(input$load,{
    shinyjs::disable("load")
    shinyjs::hide("load")
    # browser()
    output$content <- renderUI(mod_sankeyNetwork_ui("sankeyNetwork_1"))
    mod_sankeyNetwork_server("sankeyNetwork_1", data())
  })

  # observeEvent(data(),{
  #   error <- data()$ErrorMessage
  #   if(!(error %in% c("Valid data", "No data"))){
  #     output$error <- renderText(error)
  #   }else{
  #     output$error <- renderText({})
  #   }
  # })

}
