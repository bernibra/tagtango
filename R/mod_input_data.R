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
        p("The app expects a `MultiAssayExperiment` object, a `SingleCellExperiment` or `data.frame` with annotations as colData or columns, respectively."),
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
      uiOutput(ns("annotations"))
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
    values$data <- list(sce = NULL,
                        # adt = NULL, norm = NULL,
                        dat = NULL, ReadError = "No data")
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
      output$annotations <- renderUI({})
      output$additional_info <- renderUI({})

      values$data <- read_input("test_data")

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

      output$annotations <- renderUI({
        tagList(
            column(12, p("The data has been loaded! Now, you need to define the basic aspects of the comparision between annotations.")),
            column(4, align = "left", style = "padding: 1em; vertical-align: middle;",
                   column(12, class = "inner_box",
                            shinyWidgets::pickerInput(
                              ns("data_type"),labelMandatory("Data type"),
                              choices = names(MultiAssayExperiment::experiments(values$data$mae)),
                              multiple = T,
                              selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "e.g. ADT")
                            )
                          )
                   ),
            column(8, align = "left", style = "padding: 1em;",
                   column(12, class = "inner_box",
                              column(6, align = "left",
                                     shinyWidgets::pickerInput(ns("left"),labelMandatory("Annotation #1"),
                                                               choices = colnames(values$data$dat), multiple = T,
                                                               selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "e.g. Main.ADT"))
                              ),
                              column(6, align = "right",
                                     shinyWidgets::pickerInput(ns("right"),labelMandatory("Annotation #2"),
                                                               choices = colnames(values$data$dat), multiple = T,
                                                               selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "e.g. Main.RNA"))
                              ),
                          )
                   ),
        )
      })

    })

    observeEvent(input$data,{
      output$annotations <- renderUI({})
      output$additional_info <- renderUI({})

      values$data <- tryCatch({
            read_input(input$data$datapath)
          }, error = function(e) {
            read_input(NULL)
        })
      values$ReadError <- values$data$ReadError

      if(!is.null(values$data$dat)){

        if(!is.null(values$data$mae)){
          choices <- names(MultiAssayExperiment::experiments(values$data$mae))
          selected <- NULL
          multiple <- T
        }else if(!is.null(values$data$sce)){
          choices <- c("RNA", "ADT")
          selected <- NULL
          multiple <- T
        }else{
          choices <- c("No expression data")
          selected <- "No expression data"
          multiple <- F
        }

        shinyWidgets::updateRadioGroupButtons(
          session = session, inputId = "test_data",
          selected = character(0)
        )

        output$annotations <- renderUI({
          tagList(
            column(12, p("The data has been loaded! Now, you need to define the basic aspects of the comparision between annotations.")),
            column(4, align = "left", style = "padding: 1em; vertical-align: middle;",
                   column(12, class = "inner_box",
                          shinyWidgets::pickerInput(
                            ns("data_type"),labelMandatory("Data type"),
                            choices = choices,
                            multiple = multiple,
                            selected = selected, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner")
                          )
                   )
            ),
            column(8, align = "left", style = "padding: 1em;",
                   column(12, class = "inner_box",
                          column(6, align = "left",
                                 shinyWidgets::pickerInput(ns("left"),labelMandatory("Annotation #1"),
                                                           choices = colnames(values$data$dat), multiple = T,
                                                           selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner"))
                          ),
                          column(6, align = "right",
                                 shinyWidgets::pickerInput(ns("right"),labelMandatory("Annotation #2"),
                                                           choices = colnames(values$data$dat), multiple = T,
                                                           selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner"))
                          ),
                   )
            ),


          )
        })

      }

      if(!(values$data$ReadError %in% c("Valid data", "No data"))){
        shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$data$ReadError,
                               closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
      }

      if(values$data$ReadError != "Valid data"){
        output$annotations <- renderUI({})
        output$additional_info <- renderUI({})
      }

    })

    annotationsListen <- reactive({
      list(input$left, input$right, input$data_type)
    })

    observeEvent(annotationsListen(),{
      if(!(is.null(input$left) || is.null(input$right) || is.null(input$data_type))){
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
                br(),
                h4("Additional Information"),
                mod_cell_grouping_ui(ns("cell_grouping_1"), choices = colnames(values$data$dat)),
                mod_cell_filtering_ui(ns("cell_filtering_1"), choices = colnames(values$data$dat)),
                mod_input_data_UMAP_ui(ns("input_data_UMAP_1"), choices = colnames(values$data$dat)),
              )
            })
          }
        }
      }
    })

    grouping <- mod_cell_grouping_server("cell_grouping_1", dat = values$data$dat)
    filtering <- mod_cell_filtering_server("cell_filtering_1", dat = values$data$dat)
    umap <- mod_input_data_UMAP_server("input_data_UMAP_1", dat = values$data$dat)

    output$spinner <- renderUI(shiny::absolutePanel(top = "3%", right =  "3%", width = "auto", height = "auto", draggable = F, fixed = T,
                                           shiny::HTML("<span class='loader'></span>")))

    return(
      reactive(
        c(list(
          dat = values$data$dat,
          left = input$left,
          right = input$right,
          sce = if(!is.null(values$data$mae)){values$data$mae[[input$data_type]]}else if(!is.null(values$data$sce)){values$data$sce}else{NULL},
          norm = NULL,
          rna_umap = umap()$rna,
          adt_umap = umap()$adt,
          data_type = input$data_type,
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
