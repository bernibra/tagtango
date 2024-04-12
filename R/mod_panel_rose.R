#' panel_rose UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_rose_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("panel"))
  )
}

#' panel_rose Server Functions
#'
#' @noRd
mod_panel_rose_server <- function(id, values,
                                  class = "topleft white", panel_padding = 20,
                                  top = "98%", left = "1%"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataListen <- reactive({
      list(values$norm, values$first_selection, values$second_selection, values$valley_position)
    })

    observeEvent(dataListen(),{

      if(is.null(values$first_selection)){
        output$rose <- renderUI({})
        output$rose2 <- renderUI({})
        output$diff <- renderUI({})
        output$options_tab <- renderUI({})
        output$panel <- renderUI({})
      }else{
        if(is.null(values$second_selection)){
          output$panel <- renderUI({
            absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                          top = top, left = left, width = values$width, fixed=TRUE,
                          draggable = F, height = "auto",
                          fluidRow(
                            column(12,
                                   plotOutput(ns("rose"),
                                              width = values$width-panel_padding,
                                              height = values$height-panel_padding)
                            )
                          ),
                          uiOutput(ns("options_tab"))

            )
          })

          output$options_tab <- renderUI({
            tagList(
              fluidRow(column(7, offset = 5, align="right",
                              shinyWidgets::pickerInput(
                                inputId = ns("nmark"),
                                label = "",
                                choices = c(),
                                options = shinyWidgets::pickerOptions(
                                  actionsBox = FALSE,
                                  countSelectedText = "show {0}",
                                  selectedTextFormat = "count > 2"
                                ),
                                multiple = TRUE, selected = NULL
                              )
              ),
              )
            )
          })

          fexp <- values$norm[(rownames(values$norm) %in% rownames(values$network$dat)[values$first_selection]), ]
          ncell <- nrow(fexp)
          data <- find_markers(extra = 0, n = 15, mat = fexp, quant = values$quantiles, zero = values$valley_position)

          output$options_tab <- renderUI({
            tagList(
              fluidRow(column(7, offset = 5, align="right",
                              shinyWidgets::pickerInput(
                                inputId = ns("nmark"),
                                label = "",
                                choices = data$all,
                                options = shinyWidgets::pickerOptions(
                                  actionsBox = FALSE,
                                  countSelectedText = "show {0}",
                                  selectedTextFormat = "count > 2"
                                ),
                                multiple = TRUE, selected = data$selected
                              )
              ),
              )
            )
          })
          output$rose <- renderPlot({
            rose_plot_internal(data = data$data, selected = if(is.null(input$nmark)){data$selected}else{input$nmark},
                      title = ifelse(is.null(ncell), "1 cell", paste0(ncell, " cells")),
                      maintitle = values$ftitle, palette=ifelse(is.null(values$valley_position), "BuGn", "RdYlGn"),
                      valley = values$valley_position)
          }, bg="transparent", height = values$height, width = values$width-panel_padding)
        }else{
          output$panel <- renderUI({
            absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                          top = top, left = left, width = values$width, fixed=TRUE,
                          draggable = F, height = "auto",
                          shiny::tabsetPanel(
                            shiny::tabPanel("1st selection",plotOutput(ns("rose"),
                                                                  width = values$width-panel_padding,
                                                                  height = values$height-panel_padding)),
                            shiny::tabPanel("2nd selection", plotOutput(ns("rose2"),
                                                                           width = values$width-panel_padding,
                                                                           height = values$height-panel_padding)),
                            shiny::tabPanel("Difference", plotOutput(ns("diff"),
                                                                           width = values$width-panel_padding,
                                                                           height = values$height-panel_padding)),
                          ),
                          uiOutput(ns("options_tab"))
            )
          })

          # First selection
          fexp <- values$norm[(rownames(values$norm) %in% rownames(values$network$dat)[values$first_selection]), ]
          fncell <- nrow(fexp)
          fdata <- find_markers(extra = 0, n = 10, mat = fexp, quant = values$quantiles, zero = values$valley_position)

          # Second selection
          sexp <- values$norm[(rownames(values$norm) %in% rownames(values$network$dat)[values$second_selection]), ]
          sncell <- nrow(sexp)
          sdata <- find_markers(extra = 0, n = 10, mat = sexp, quant = values$quantiles, zero = values$valley_position)

          #Difference
          data_diff <- find_markers_diff(extra = 0, n = 10, mat_left = fexp, mat_right = sexp, quant = values$quantiles, zero = values$valley_position)
          data_diff_ <- find_markers_diff_PI(mat_left = fexp, mat_right = sexp)

          output$options_tab <- renderUI({
            tagList(
              fluidRow(column(7, offset = 5, align="right",
                              shinyWidgets::pickerInput(
                                inputId = ns("nmark"),
                                label = "",
                                choices = data_diff$all,
                                options = shinyWidgets::pickerOptions(
                                  actionsBox = FALSE,
                                  countSelectedText = "show {0}",
                                  selectedTextFormat = "count > 2"
                                ),
                                multiple = TRUE, selected = data_diff$selected
                              )
              ),
              )
            )
          })
          output$rose <- renderPlot({
            rose_plot_internal(data = fdata$data, selected = if(is.null(input$nmark)){data_diff$selected}else{input$nmark},
                      title = ifelse(is.null(fncell), "1 cell", paste0(fncell, " cells")),
                      maintitle = values$ftitle, palette=ifelse(is.null(values$valley_position), "BuGn", "RdYlGn"),
                      valley = values$valley_position)
          }, bg="transparent", height = values$height, width = values$width-panel_padding)
          output$rose2 <- renderPlot({
            rose_plot_internal(data = sdata$data, selected = if(is.null(input$nmark)){data_diff$selected}else{input$nmark},
                      title = ifelse(is.null(sncell), "1 cell", paste0(sncell, " cells")),
                      maintitle = values$stitle, palette=ifelse(is.null(values$valley_position), "YlOrBr", "RdYlGn"),
                      valley = values$valley_position)
          }, bg="transparent", height = values$height, width = values$width-panel_padding)
          output$diff <- renderPlot({box_diff_internal(data = data_diff_,
                     selected = if(is.null(input$nmark)){data_diff$selected}else{input$nmark},
                     values = c("first"="#35978f", "second"="#bf812d"),
                     title = NULL, maintitle = "Differences: <span style = 'color:#35978f;'>**first**</span> vs <span style = 'color:#bf812d;'>**second**</span> selection",
                     palette="BrBG", colortitle = TRUE)
          }, bg="transparent", height = values$height, width = values$width-panel_padding)
        }
      }
    }, ignoreInit = TRUE, priority = 9, ignoreNULL = TRUE)


  })
}

## To be copied in the UI
# mod_panel_rose_ui("panel_rose_1")

## To be copied in the server
# mod_panel_rose_server("panel_rose_1")
