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
    div(style = "padding: 1em; padding-top: 0em;",
      tags$details(
        tags$summary("Dimension reduction"),
          column(6, align = "left", style = "padding: 1em; vertical-align: middle;",
           column(12, class = "inner_box",
                  p("First data projection (e.g. RNA projection)"),
                  column(6,
                         shinyWidgets::pickerInput(ns("RNA_first_axis"),"First axis:",
                                                   choices = choices, multiple = T,
                                                   selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "None"))
                  ),
                  column(6,
                         shinyWidgets::pickerInput(ns("RNA_second_axis"),"Second axis:",
                                                   choices = choices, multiple = T,
                                                   selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "None"))
                  )
          ),
        ),
        column(6, align = "left", style = "padding: 1em;",
          column(12, class = "inner_box",
                 p("Second data projection (e.g. ADT projection)"),
                 column(6,
                        shinyWidgets::pickerInput(ns("ADT_first_axis"),"First axis:",
                                                  choices = choices, multiple = T,
                                                  selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "None"))
                 ),
                 column(6,
                        shinyWidgets::pickerInput(ns("ADT_second_axis"),"Second axis:",
                                                  choices = choices, multiple = T,
                                                  selected = NULL, options = shinyWidgets::pickerOptions(maxOptions = 1, style = "custom-inner", title = "None"))
                 )
          ),
        ),
      )
    )
  )
}

#' input_data_UMAP Server Functions
#'
#' @noRd
mod_input_data_UMAP_server <- function(id, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()

    values$rna <- NULL
    values$adt <- NULL

    rnaListen <- reactive({
      list(input$RNA_first_axis, input$RNA_second_axis)
    })

    adtListen <- reactive({
      list(input$ADT_first_axis, input$ADT_second_axis)
    })

    observeEvent(rnaListen(),{
      if(!(is.null(input$RNA_first_axis) || is.null(input$RNA_second_axis))){
        # Run checks for data
        values$rna <- data.frame(V1 = dat[,input$RNA_first_axis], V2 = dat[,input$RNA_second_axis])
      }else{
        values$rna <- NULL
        values$RNAtitle <- NULL
      }
    })

    observeEvent(adtListen(),{
      if(!(is.null(input$ADT_first_axis) || is.null(input$ADT_second_axis))){
        # Run checks for data
        values$adt <- data.frame(V1 = dat[,input$ADT_first_axis], V2 = dat[,input$ADT_second_axis])
      }else{
        values$adt <- NULL
        values$ADTtitle <- NULL
      }
    })

    return(
      reactive(
        list(
          rna_umap = values$rna,
          adt_umap = values$adt,
          ADTtitle = find_name_projection(n=2,
                                          P1a1 = input$ADT_first_axis,
                                          P1a2 = input$ADT_second_axis,
                                          P2a1 = input$RNA_first_axis,
                                          P2a2 = input$RNA_second_axis),
          RNAtitle = find_name_projection(n=1, P1a1 = input$RNA_first_axis,
                                          P1a2 = input$RNA_second_axis,
                                          P2a1 = input$ADT_first_axis,
                                          P2a2 = input$ADT_second_axis),
          codebit = paste0(", pc1_axis1 = ", rsym(input$RNA_first_axis),
                           ", pc1_axis2 = ", rsym(input$RNA_second_axis),
                           ", pc2_axis1 = ", rsym(input$ADT_first_axis),
                           ", pc2_axis2 = ", rsym(input$ADT_second_axis))
          )
        )
      )
  })
}

## To be copied in the UI
# mod_input_data_UMAP_ui("input_data_UMAP_1")

## To be copied in the server
# mod_input_data_UMAP_server("input_data_UMAP_1")
