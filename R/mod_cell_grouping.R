#' cell_grouping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cell_grouping_ui <- function(id, choices){
  ns <- NS(id)
  tagList(
    div(style = "padding: 1em;",
        tags$details(
          tags$summary("Cell organization"),
          column(12, align = "left", style = "padding: 1em; vertical-align: middle;",
                 p("Would you like to group cells by batches, samples, or main cell types? (max 100 unique groups)."),
                 column(4, style = "padding: 0em;",
                        shinyWidgets::pickerInput(ns("grouping"),"Pick a variable:",
                                                  choices = choices, multiple = T,
                                                  selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom", title = "None"))
                        )
          ),
      )
    )
  )
}

#' cell_grouping Server Functions
#'
#' @noRd
mod_cell_grouping_server <- function(id, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()
    values$variable <- NULL
    values$values <- NULL

    dataListen <- reactive({
      list(input$grouping)
    })

    observeEvent(dataListen(), {
      if(!is.null(input$grouping)){
        choices <- unique(dat[,input$grouping])
        # print(choices)
        if(length(choices)>100){
          values$variable <- NULL
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = "There are too many unique values to group cells by this variable.",
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else if(length(choices)<2){
          values$variable <- NULL
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = "This variable has only one unique value.",
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else{
          values$variable <- input$grouping
        }
      }
    })

    return(
      reactive(
        list(
          grouping_variable = values$variable
        )
      )
    )

  })
}

## To be copied in the UI
# mod_cell_grouping_ui("cell_grouping_1")

## To be copied in the server
# mod_cell_grouping_server("cell_grouping_1")
