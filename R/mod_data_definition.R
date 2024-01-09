#' data_definition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_definition_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("data_ui"))
  )
}

#' data_definition Server Functions
#'
#' @noRd
mod_data_definition_server <- function(id, show = T){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()
    values$datatype <- "None"

    if(show){
      output$data_ui <- renderUI({
        div(style = "padding: 1em; padding-top: 0em;",
            tags$details(
              tags$summary("Expression data"),
              column(12, align = "left", style = "padding: 1em; vertical-align: middle;",
                     p("The file uploaded contains a normalized expression matrix. What type of data is it?"),
                     column(4, style = "padding: 0em;",
                            shinyWidgets::switchInput(
                              inputId = ns("data_type"),
                              label = "Data type",
                              size = "mini",
                              labelWidth = "80px",
                              onLabel = "RNA",
                              offLabel = "ADT",
                              onStatus = "custom",
                              offStatus = "custom"
                            )
                     )
              ),
            )
        )
      })
    }else{
      output$data_ui <- renderUI({})
      values$datatype <- "None"
    }

    observeEvent(input$data_type, {
      if(input$data_type){
        values$datatype <- "RNA"
      }else{
        values$datatype <- "ADT"
      }
    })

    return(
      reactive(
        values$datatype
      )
    )
  })
}

## To be copied in the UI
# mod_data_definition_ui("data_definition_1")

## To be copied in the server
# mod_data_definition_server("data_definition_1")
