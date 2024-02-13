#' normalization_specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_normalization_specs_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(style = "padding: 1em;",
        tags$details(
          tags$summary("Normalization specifications"),
          column(12, align = "left", style = "padding: 1em; vertical-align: middle;",
                 p("Is the data normalized such that positive and negative peaks are aligned?."),
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

#' normalization_specs Server Functions
#'
#' @noRd
mod_normalization_specs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_normalization_specs_ui("normalization_specs_1")

## To be copied in the server
# mod_normalization_specs_server("normalization_specs_1")
