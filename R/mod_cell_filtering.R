#' cell_filtering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cell_filtering_ui <- function(id, choices){
  ns <- NS(id)
  tagList(
    div(style = "padding: 1em; padding-top: 0em;",
        tags$details(
          tags$summary("Cell filtering"),
          column(12, align = "left", style = "padding: 1em;",
                 p("You can filter out certain cells based on any of the existing variables (e.g. undefined)."),
                 column(5, style = "padding: 0em;",
                        shinyWidgets::pickerInput(ns("filter_variable"),"Pick a variable:",
                                                  choices = choices, multiple = T,
                                                  selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom", title = "None"))
                        ),
                 column(5, offset=2, style = "padding: 0em;",
                        uiOutput(ns("filter_variable_holder"))
                        )
          ),
        )
    )
  )
}

#' cell_filtering Server Functions
#'
#' @noRd
mod_cell_filtering_server <- function(id, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()
    values$variable <- NULL
    values$values <- NULL

    dataListen <- reactive({
      list(input$filter_variable, dat)
    })

    observeEvent(dataListen(), {
      if(!is.null(input$filter_variable)){
        choices <- NAorNANcheck(dat$data$dat[,input$filter_variable])
        choices <- unique(choices)

        if(length(choices)>1000){
          values$variable <- NULL
          output$filter_variable_holder <- renderUI({})
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = "There are too many unique values to display this variable.",
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else if(length(choices)<2){
          values$variable <- NULL
          output$filter_variable_holder <- renderUI({})
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = "This variable has only one unique value; you can't filter all cells.",
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else if(any(is.na(choices)) | any(is.nan(choices))){
          values$variable <- NULL
          output$filter_variable_holder <- renderUI({})
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = "The filtering variable cannot contain NA or NaN values; modify the object or change the variable and try again",
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else{
          values$variable <- input$filter_variable
          output$filter_variable_holder <- renderUI({
            shinyWidgets::pickerInput(ns("filter_values"),"Which ones should be excluded?",
                                      choices = choices, multiple = T,
                                      selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = length(choices)-1, style = "custom", title = "None"))
          })
        }
      }else{
        values$variable <- NULL
      }
    })

    datavalueListen <- reactive({
      list(input$filter_values)
    })

    observeEvent(datavalueListen(), {
      if(!is.null(input$filter_values)){
        values$values <- input$filter_values
      }else{
        values$values <- NULL
      }
    })

    return(
      reactive(
        list(filter_variable = values$variable,
             filter_values = values$values)
      )
    )

  })
}

## To be copied in the UI
# mod_cell_filtering_ui("cell_filtering_1")

## To be copied in the server
# mod_cell_filtering_server("cell_filtering_1")
