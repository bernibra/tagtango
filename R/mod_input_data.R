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
    fluidRow(
      column(10, offset = 1,
             div(style="width:100%; justify-content: center;",
                 div(style="text-align: justify;", p("This is a web app for comparing annotations in single-cell data. The app expects a `SingleCellExperiment` object or `data.frame` with multiple annotations."))
             ))
    ),
    br(),
    fluidRow(
      column(6, align = "left",
            shiny::fileInput(
               ns("data"),
               "rds object:",
               multiple = FALSE,
               accept = NULL,
               width = NULL,
               buttonLabel = "Browse...",
               placeholder = "No file selected",
               capture = NULL
             ),
      ),
    ),
    fluidRow(
      column(4, align = "left",
             uiOutput(ns("left_input"))
      ),
      column(4, offset = 4, align = "right",
             uiOutput(ns("right_input"))
      ),
    ),
    fluidRow(
      uiOutput(ns("UMAP"))
    ),

  )
}

#' input_data Server Functions
#'
#' @noRd
mod_input_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()
    values$data <- list(adt = NULL, norm = NULL, dat = NULL, ReadError = "No data")
    values$umap <- data.frame(rna_first = NULL, rna_second = NULL, adt_first = NULL, adt_second = NULL)
    values$ReadError <- "No data"

    shinyjs::disable("load", asis = T)

    dataListen <- reactive({
      list(input$data)
    })

    observeEvent(dataListen(),{
      print("Data read")
      values$data <- read_input(input$data$datapath)
      values$ReadError <- values$data$ReadError

      if(!is.null(values$data$dat)){
        output$left_input <- renderUI(shinyWidgets::pickerInput(ns("left"),"annotation #1:",
                                          choices = colnames(values$data$dat), multiple = T,
                                          selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom")))
        output$right_input <- renderUI(shinyWidgets::pickerInput(ns("right"),"annotation #2:",
                                          choices = colnames(values$data$dat), multiple = T,
                                          selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom")))
      }

    })

    annotationsListen <- reactive({
      list(input$left, input$right)
    })

    umap <- mod_input_data_UMAP_server("input_data_UMAP_1")

    observeEvent(annotationsListen(),{
      if(!(is.null(input$left) || is.null(input$right))){
        maxlabels <- max(c(length(unique(values$data$dat[,input$left])),length(unique(values$data$dat[,input$right]))))

        if(maxlabels>1000){
          values$ReadError <- "One of the annotations have more than 1000 labels. That seems unreasonable..."
        }else{
          values$ReadError <- "Valid data"
          shinyjs::enable("load", asis = T)
          output$UMAP <- renderUI(mod_input_data_UMAP_ui(ns("input_data_UMAP_1"), choices = colnames(values$data$dat)))
        }
      }
    })

    return(
      reactive(
        list(
          annotation_left = input$left,
          annotation_right = input$right,
          umap = umap(),
          ErrorMessage = values$ReadError
        )
      )
    )
  })
}

## To be copied in the UI
# mod_input_data_ui("input_data_1")

## To be copied in the server
# mod_input_data_server("input_data_1")
