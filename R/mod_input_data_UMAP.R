#' input_data_UMAP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_data_UMAP_ui <- function(id, choices){
  ns <- NS(id)
  tagList(
    column(6, align = "left",
           p("RNA data projection"),
           column(6,
                  shinyWidgets::pickerInput(ns("RNA_first_axis"),"First axis:",
                                            choices = choices, multiple = T,
                                            selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom"))
           ),
           column(6,
                  shinyWidgets::pickerInput(ns("RNA_second_axis"),"Second axis:",
                                            choices = choices, multiple = T,
                                            selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom"))
           )
    ),
    column(6, align = "left",
           p("ADT data projection"),
           column(6,
                  shinyWidgets::pickerInput(ns("ADT_first_axis"),"First axis:",
                                            choices = choices, multiple = T,
                                            selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom"))
           ),
           column(6,
                  shinyWidgets::pickerInput(ns("ADT_second_axis"),"Second axis:",
                                            choices = choices, multiple = T,
                                            selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom"))
           )
    ),
  )
}

#' input_data_UMAP Server Functions
#'
#' @noRd
mod_input_data_UMAP_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$RNA_first_axis, {
      print(input$RNA_first_axis)
    })

    return(
      reactive(
        list(rna_first = input$RNA_first_axis, rna_second = input$RNA_second_axis, adt_first = input$ADT_first_axis, adt_second = input$ADT_second_axis)
        )
      )
  })
}

## To be copied in the UI
# mod_input_data_UMAP_ui("input_data_UMAP_1")

## To be copied in the server
# mod_input_data_UMAP_server("input_data_UMAP_1")
