#' configuration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_configuration_ui <- function(id, default_value){
  ns <- NS(id)
  tagList(
    div(style = "padding: 1em; padding-top: 0em;",
        tags$details(
          tags$summary("Configuration"),
          column(12, align = "left", style = "padding: 1em; vertical-align: middle;",
                 p("How should the data be processed?"),
                 div(class ="outerDiv_container",
                     div(class = "outerDiv",
                          column(2, align = "right",
                           htmltools::tagAppendAttributes(
                             shinyWidgets::switchInput(
                               value = if(is.null(default_value)){TRUE}else{default_value},
                               inputId = ns("lowdimension"),
                               onLabel = "Low",
                               label = "dimension",
                               offLabel = "High",
                               labelWidth = "80px",
                               onStatus = "low",
                               offStatus = "high",
                             ),
                             style = "text-align: initial;"
                            )
                          ),
                          column(10, align="left",
                              shiny::textOutput(ns("observation"))
                          ),
                        )
                      )
            ),
        )
    )
  )
}

#' configuration Server Functions
#'
#' @noRd
mod_configuration_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()

    values$text <- "default text"
    values$dimension <- 0

    dataListen <- reactive({
      list(input$lowdimension, data$default_configuration)
    })

    observeEvent(dataListen(), {
      if(!is.null(input$lowdimension)){
        if(input$lowdimension){
          values$text <- "A low-dimensional approach will use all features in the expression data for comparing annotations/clusters. This is designed for low-dimensional data types, such as surface protein expression data (ADT), and it does not involve any pre-processing of the data."
          values$dimension <- 1
        }else{
          values$text <- "The application will first use the function `scoreMarkers' from the R package `scran' to quantify the expression differences across every possible annotation/cluster, computing summary scores for each marker in each group of cells. Then, it will select the 10 most upregulated markers for every group using the median Cohen’s d."
          values$dimension <- 2
        }
      }

      if(is.null(data$default_configuration)){
        values$dimension <- 0
      }

    }, priority = 50)

    output$observation <- renderText(values$text)

    return(
      reactive(
        list(
          dimension = values$dimension
        )
      )
    )

  })
}

## To be copied in the UI
# mod_configuration_ui("configuration_1")

## To be copied in the server
# mod_configuration_server("configuration_1")
