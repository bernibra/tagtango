#' panel_diff UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_diff_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("panel"))
  )
}

#' panel_diff Server Functions
#'
#' @noRd
mod_panel_diff_server <- function(id, data, width=300, height=300, class = "top white", top = "1%", left = "1%"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$panel <- renderUI({
      absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                    top = top, right = left, width = width, fixed=TRUE,
                    draggable = TRUE, height = height,
                    uiOutput(ns("text"))
      )
    })

    dataListen <- reactive(
      list(data)
    )

    observeEvent(dataListen(), {
      if(!is.null(data)){
        output$text <- renderUI({h1(data)})
      }else{
        output$text <- renderUI({h1("NULL")})
      }
    })

  })
}

## To be copied in the UI
# mod_panel_diff_ui("panel_diff_1")

## To be copied in the server
# mod_panel_diff_server("panel_diff_1")
