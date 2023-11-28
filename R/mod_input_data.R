#' input_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = "centerContainer",
        fluidRow(
          fluidRow(
            column(10, offset = 1,
                   div(style="width:100%; justify-content: center;",
                       h2("Comparing different annotations"),
                       div(style="text-align: justify;", p("This is a web app for comparing annotations in single-cell data. The app expects a `SingleCellExperiment` object or `data.frame` with multiple annotations."))
                   ))
          ),
          br(),
          br(),
          fluidRow(
            column(12, align = "center",
                  shiny::fileInput(
                     "data",
                     "rds object:",
                     multiple = FALSE,
                     accept = NULL,
                     width = NULL,
                     buttonLabel = "Browse...",
                     placeholder = "No file selected",
                     capture = NULL
                   ),
            ),
            fluidRow(
              column(12, align = "right",
                     shiny::actionButton(outputId = "load", label = "Load data", class = "custom")
                     )
            )
          )
        )
    )
  )
}

#' input_data Server Functions
#'
#' @noRd
mod_input_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_input_data_ui("input_data_1")

## To be copied in the server
# mod_input_data_server("input_data_1")
