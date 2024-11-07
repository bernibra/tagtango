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
        uiOutput(ns("data_message")),
        div(class ="outerDiv_container",
            div(class = "outerDiv", style = "align-items: flex-start; justify-content: flex-start;",
                uiOutput(ns("data_holder")),
                div(style = "margin: 0px; padding: 0px; padding-left: 2pt;",
                 uiOutput(ns("test_data_holder")),
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
                        dat = NULL, ReadError = "No data")

    values$umap <- data.frame(rna_first = NULL, rna_second = NULL, adt_first = NULL, adt_second = NULL)
    values$ReadError <- "No data"
    values$code <- "library(tagtango)\n\n"

    values$loading_ui <- if(is.null(golem::get_golem_options(which = "input_data"))){TRUE}else{NULL}
    values$inputfile <- if(is.null(golem::get_golem_options(which = "input_data"))){NULL}else{list(datapath="argument")}

    shinyjs::disable("load", asis = T)

    observeEvent(values$loading_ui, {
      output$data_message <- renderUI({
        p("The app expects a `MultiAssayExperiment` object, a `SingleCellExperiment` or `data.frame` with annotations as colData or columns, respectively.")
      })

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

      output$test_data_holder <- renderUI({
        shinyWidgets::radioGroupButtons(
          inputId = ns("test_data"),
          label = "",
          selected = character(0),
          choices = c("Test data"),
          status = "custom"
        )
      })
    })

    observeEvent(input$test_data,{
      output$annotations <- renderUI({})
      output$additional_info <- renderUI({})

      values$data <- isolate(read_input(filename = "test_data", run_test_data = TRUE))
      values$filename <- NULL

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

      if(is.null(values$data$mae)){
        choices <- c("ADT")
        selected <- "ADT"
        multiple <- F
      }else{
        choices <- names(values$data$mae)
        selected <- NULL
        multiple <- T
      }

      output$annotations <- renderUI({
        tagList(
            column(12, p("A dataset was loaded! Now, you need to define the basic aspects of the comparision between annotations.")),
            column(4, align = "left", style = "padding: 1em; vertical-align: middle;",
                   column(12, class = "inner_box",
                            shinyWidgets::pickerInput(
                              ns("data_type"),labelMandatory("Experiment"),
                              choices = choices,
                              multiple = multiple,
                              selected = selected, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "e.g. ADT")
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
      values$inputfile <- input$data
    })

    observeEvent(values$inputfile,{
      output$annotations <- renderUI({})
      output$additional_info <- renderUI({})

      values$data <- tryCatch({
            read_input(filename = values$inputfile$datapath, data = golem::get_golem_options(which = "input_data"))
          }, error = function(e) {
            read_input(NULL)
        })

      values$ReadError <- values$data$ReadError
      values$filename <- input$data$datapath

      # Spit out warning if any
      if(values$data$warning!=""){
        shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$data$warning,
                               closeOnClickOutside = T, closeOnEsc = T, animation = "pop",
                               confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
      }

      # Check out the data
      if(!is.null(values$data$dat)){

        if(!is.null(values$data$mae)){
          choices <- names(values$data$mae)
          selected <- NULL
          multiple <- T
        }else if(!is.null(values$data$sce)){
          choices <- c("Undefined expression data")
          selected <- "Undefined expression data"
          multiple <- F
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
            column(12, p("A dataset was loaded! Now, you need to define the basic aspects of the comparision between annotations.")),
            column(4, align = "left", style = "padding: 1em; vertical-align: middle;",
                   column(12, class = "inner_box",
                          shinyWidgets::pickerInput(
                            ns("data_type"),labelMandatory("Experiment"),
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
                               closeOnClickOutside = T, closeOnEsc = T, animation = "pop",
                               confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")

        # if(values$data$ReadError == "Wrong object type"){
          values$loading_ui <- TRUE
          values$inputfile <- NULL
        # }

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

        values$data_type <- input$data_type

        if(!is.null(values$data$mae)){
          values$default_configuration <- ifelse(nrow(values$data$mae[[values$data_type]])<=500, TRUE, FALSE)
        }else if(!is.null(values$data$sce)){
          values$default_configuration <- ifelse(nrow(values$data$sce)<=500, TRUE, FALSE)
        }else{
          values$default_configuration <- NULL
        }

        # Find unique values and run checks
        unique_left <- NAorNANcheck(values$data$dat[,input$left])
        unique_left <- unique(unique_left)
        unique_right <- NAorNANcheck(values$data$dat[,input$right])
        unique_right <- unique(unique_right)
        maxlabels <- max(c(length(unique_left),length(unique_right)))

        # Old check but worth keeping
        NAorNaN <- any(c(any(is.na(unique_left)), any(is.nan(unique_left)),
                         any(is.na(unique_right)), any(is.nan(unique_right))))

        if(maxlabels>1000){
          values$ReadError <- "One of the two annotations have more than 1000 labels. Change it to something more managable as this seems unreasonable..."
          output$additional_info <- renderUI({})
          shinyjs::disable("load", asis = T)
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$ReadError,
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else if(maxlabels<=1){
          values$ReadError <- "Both annotations have less than 2 labels. Change at least one of them to an annotation with multiple labels."
          output$additional_info <- renderUI({})
          shinyjs::disable("load", asis = T)
          shinyalert::shinyalert(title = "Oups!", type = "warning", text = values$ReadError,
                                 closeOnClickOutside = T, closeOnEsc = T, animation = "pop", confirmButtonText = "Got it", className = "warning_popup", confirmButtonCol = "#909097")
        }else if(NAorNaN){
          values$ReadError <- "One or both annotations contain NA or NaN values. Modify the object, turning these into strings, or change the annotations."
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
            if(is.null(values$default_configuration)){
              output$additional_info <- renderUI({
                tagList(
                  br(),
                  h4("Additional Information"),
                  mod_cell_grouping_ui(ns("cell_grouping_1"), choices = colnames(values$data$dat)),
                  mod_cell_filtering_ui(ns("cell_filtering_1"), choices = colnames(values$data$dat)),
                  mod_input_data_UMAP_ui(ns("input_data_UMAP_1"), choices = colnames(values$data$dat))
                )
              })
            }else{
              output$additional_info <- renderUI({
                tagList(
                  br(),
                  h4("Additional Information"),
                  mod_cell_grouping_ui(ns("cell_grouping_1"), choices = colnames(values$data$dat)),
                  mod_cell_filtering_ui(ns("cell_filtering_1"), choices = colnames(values$data$dat)),
                  mod_input_data_UMAP_ui(ns("input_data_UMAP_1"), choices = colnames(values$data$dat)),
                  mod_configuration_ui(ns("configuration_1"), default_value = values$default_configuration)
                )
              })
            }
          }
        }
      }else{
        output$additional_info <- renderUI({})
        shinyjs::disable("load", asis = T)
      }
    }, priority = 100)

    configuration <- mod_configuration_server("configuration_1", data = values)
    grouping <- mod_cell_grouping_server("cell_grouping_1", dat = values)
    filtering <- mod_cell_filtering_server("cell_filtering_1", dat = values)
    umap <- mod_input_data_UMAP_server("input_data_UMAP_1", dat = values)

    output$spinner <- renderUI(shiny::absolutePanel(top = "3%", right =  "3%", width = "auto", height = "auto", draggable = F, fixed = T,
                                           shiny::HTML("<span class='loader'></span>")))

    return(
      reactive(
        c(list(
          code = paste0(values$code,
                        ifelse(!is.null(values$loading_ui), "", "# the object `input_data' should point at the input data object\n\n"),
                        "dat <- process_data(filename = ", rsym(values$filename),
                        ", input_data = ", ifelse(!is.null(values$loading_ui), "NULL", "input_data"),
                        ", data_type = ", rsym(values$data_type),
                        ", left = ", rsym(input$left),
                        ", right = ", rsym(input$right)),
          dat = values$data$dat,
          left = input$left,
          right = input$right,
          norm = if(!is.null(values$data$mae)){values$data$mae[[values$data_type]]}else if(!is.null(values$data$sce)){values$data$sce}else{NULL},
          slow = FALSE,
          data_type = values$data_type,
          ErrorMessage = values$ReadError
        ),
        configuration(),
        umap(),
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
