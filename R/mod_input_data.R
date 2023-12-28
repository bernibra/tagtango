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
      column(12,
             div(style="width:100%; justify-content: center;",
                 div(style="text-align: justify;", p("Welcome to our web app for comparing annotations for single-cell data. Trying to compare annotations generated using different strategies/data is akin to orchestrating a dance-off among immune cells \u2014 everyone's moving in different directions, some are doing their own interpretative dance, and the floor transforms into a captivating whirlwind of immune activity. It's a genomic dance party where the only rule is that there are no rules, and the real puzzle is discovering who brought salsa to the immune tango showdown. Let the annotation extravaganza begin!"))
             ))
    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     uiOutput(ns("spinner")),
    ),
    fluidRow(
      br(),
      h4("Main data entry"),
      br(),
      column(12, align = "left",
        p("The app expects a `SingleCellExperiment` object or `data.frame` with annotations as colData or columns, respectively."),
        div(class ="outerDiv_container",
            div(class = "outerDiv", style = "align-items: flex-start; justify-content: flex-start;",
                uiOutput(ns("data_holder")),
                div(style = "margin: 0px; padding: 0px; padding-left: 2pt;",
                 shinyWidgets::radioGroupButtons(
                   inputId = ns("test_data"),
                   label = "",
                   selected = character(0),
                   choices = c("Test data"),
                   status = "custom"
                 )
               )
          )
        ),
      )
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
      uiOutput(ns("additional_info"))
    )
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

    output$data_holder <- renderUI({
      shiny::fileInput(
        ns("data"),
        label = "",
        multiple = FALSE,
        accept = c(".csv", ".tsv", ".txt", ".rds", ".Rds"),
        width = NULL,
        buttonLabel = "Browse...",
        placeholder = "No file selected",
        capture = NULL
      )
    })

    observeEvent(input$test_data,{

      coldat <- as.data.frame(SingleCellExperiment::colData(test_data))
      if(length(SingleCellExperiment::reducedDimNames(test_data))!=0){
        for(i in SingleCellExperiment::reducedDimNames(test_data)){
          d <- as.data.frame(reducedDim(test_data, type = i)[,1:2])
          colnames(d) <- paste0(i, c("_first_axis", "_second_axis"))
          coldat <- cbind(coldat, d)
        }
      }

      values$data <- list(
          adt = t(as.matrix(SingleCellExperiment::counts(test_data))),
          norm = t(as.matrix(SingleCellExperiment::logcounts(test_data))),
          dat = coldat,
          ReadError = "Valid data"
        )

      output$data_holder <- renderUI({
        shiny::fileInput(
          ns("data"),
          label = "",
          multiple = FALSE,
          accept = c(".csv", ".tsv", ".txt", ".rds", ".Rds"),
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected",
          capture = NULL
        )
      })

      output$left_input <- renderUI(shinyWidgets::pickerInput(ns("left"),labelMandatory("annotation #1"),
                                                              choices = colnames(values$data$dat), multiple = T,
                                                              selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom")))
      output$right_input <- renderUI(shinyWidgets::pickerInput(ns("right"),labelMandatory("annotation #2"),
                                                               choices = colnames(values$data$dat), multiple = T,
                                                               selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom")))
    })

    observeEvent(input$data,{

      values$data <- read_input(input$data$datapath)
      values$ReadError <- values$data$ReadError

      if(!is.null(values$data$dat)){

        updateRadioGroupButtons(
          session = session, inputId = "test_data",
          selected = character(0)
        )

        output$left_input <- renderUI(shinyWidgets::pickerInput(ns("left"),labelMandatory("annotation #1"),
                                          choices = colnames(values$data$dat), multiple = T,
                                          selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom")))
        output$right_input <- renderUI(shinyWidgets::pickerInput(ns("right"),labelMandatory("annotation #2"),
                                          choices = colnames(values$data$dat), multiple = T,
                                          selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom")))
      }

      if(!(values$data$ReadError %in% c("Valid data", "No data"))){
        shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$data$ReadError,
                               closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
      }

      if(values$data$ReadError != "Valid data"){
        output$left_input <- renderUI({})
        output$right_input <- renderUI({})
        output$additional_info <- renderUI({})
      }

    })

    annotationsListen <- reactive({
      list(input$left, input$right)
    })

    observeEvent(annotationsListen(),{
      if(!(is.null(input$left) || is.null(input$right))){
        maxlabels <- max(c(length(unique(values$data$dat[,input$left])),length(unique(values$data$dat[,input$right]))))

        if(maxlabels>1000){
          values$ReadError <- "One of the two annotations have more than 1000 labels. Change it to something more managable as this seems unreasonable..."
          output$additional_info <- renderUI({})
          shinyjs::disable("load", asis = T)
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$ReadError,
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else{
          if(input$left==input$right){
            values$ReadError <- "You chose the same annotation for both variables. Change it to something different..."
            output$additional_info <- renderUI({})
            shinyjs::disable("load", asis = T)
            shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$ReadError,
                                   closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
          }else{
            values$ReadError <- "Valid data"
            shinyjs::enable("load", asis = T)
            output$additional_info <- renderUI({
              tagList(
                h4("Additional Information"),
                mod_input_data_UMAP_ui(ns("input_data_UMAP_1"), choices = colnames(values$data$dat)),
                mod_cell_grouping_ui(ns("cell_grouping_1"), choices = colnames(values$data$dat)),
                mod_cell_filtering_ui(ns("cell_filtering_1"), choices = colnames(values$data$dat)),
              )
            })
          }
        }
      }
    })

    umap <- mod_input_data_UMAP_server("input_data_UMAP_1", dat = values$data$dat)
    grouping <- mod_cell_grouping_server("cell_grouping_1", dat = values$data$dat)
    filtering <- mod_cell_filtering_server("cell_filtering_1", dat = values$data$dat)

    output$spinner <- renderUI(shiny::absolutePanel(top = "3%", right =  "3%", width = "auto", height = "auto", draggable = F, fixed = T,
                                           shiny::HTML("<span class='loader'></span>")))



    return(
      reactive(
        c(list(
          dat = values$data$dat,
          left = input$left,
          right = input$right,
          adt = values$data$adt,
          norm = values$data$norm,
          rna_umap = umap()$rna,
          adt_umap = umap()$adt,
          ErrorMessage = values$ReadError
        ),
        grouping(),
        filtering()
        )
      )
    )
  })
}

## To be copied in the UI
# mod_input_data_ui("input_data_1")

## To be copied in the server
# mod_input_data_server("input_data_1")
