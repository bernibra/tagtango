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
mod_panel_rose_server <- function(id, adt, dat, fselection, sselection = NULL, ftitle = "title", stitle = "title", class = "topleft white",
                                  height = 350, width = 350, panel_padding = 20,
                                  top = "98%", left = "1%", isRNA = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataListen <- reactive({
      list(adt, fselection, sselection)
    })

    observeEvent(dataListen(),{
      if(is.null(adt)){
        output$rose <- renderUI({})
        output$rose2 <- renderUI({})
        output$diff <- renderUI({})
        output$options_tab <- renderUI({})
        output$panel <- renderUI({})
      }else{

        if(is.null(sselection)){
          output$panel <- renderUI({
            absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                          top = top, left = left, width = width, fixed=TRUE,
                          draggable = F, height = "auto",
                          fluidRow(
                            column(12,
                                   plotOutput(ns("rose"),
                                              width = width-panel_padding,
                                              height = height-panel_padding)
                            )
                          ),
                          uiOutput(ns("options_tab"))
            )
          })
          fexp <- adt[(rownames(adt) %in% rownames(dat)[fselection]), ]
          ncell <- nrow(fexp)
          data <- find_markers(extra = 0, n = 15, mat = fexp, zero = ifelse(isRNA, mean(colMeans(fexp)), 3))

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
            rose_plot(data = data$data, selected = if(is.null(input$nmark)){data$selected}else{input$nmark},
                      title = paste0(ncell, " cells"),
                      maintitle = ftitle, palette="RdYlGn")
          }, bg="transparent", height = height, width = width-panel_padding)
        }else{
          output$panel <- renderUI({
            absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                          top = top, left = left, width = width, fixed=TRUE,
                          draggable = F, height = "auto",
                          shiny::tabsetPanel(
                            shiny::tabPanel("1st selection",plotOutput(ns("rose"),
                                                                  width = width-panel_padding,
                                                                  height = height-panel_padding)),
                            shiny::tabPanel("2nd selection", plotOutput(ns("rose2"),
                                                                           width = width-panel_padding,
                                                                           height = height-panel_padding)),
                            shiny::tabPanel("Difference", plotOutput(ns("diff"),
                                                                           width = width-panel_padding,
                                                                           height = height-panel_padding)),
                          ),
                          uiOutput(ns("options_tab"))
            )
          })

          # First selection
          fexp <- adt[(rownames(adt) %in% rownames(dat)[fselection]), ]
          fncell <- nrow(fexp)
          fdata <- find_markers(extra = 0, n = 10, mat = fexp, zero = ifelse(isRNA, mean(colMeans(fexp)), 3))

          # Second selection
          sexp <- adt[(rownames(adt) %in% rownames(dat)[sselection]), ]
          sncell <- nrow(sexp)
          sdata <- find_markers(extra = 0, n = 10, mat = sexp, zero = ifelse(isRNA, mean(colMeans(fexp)), 3))

          #Difference
          data_diff <- find_markers_diff(extra = 0, n = 10, mat_left = fexp, mat_right = sexp, zero = ifelse(isRNA, mean(colMeans(fexp)), 3))
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
            rose_plot(data = fdata$data, selected = if(is.null(input$nmark)){data_diff$selected}else{input$nmark},
                      title = paste0(fncell, " cells"),
                      maintitle = ftitle, palette="RdYlGn")
          }, bg="transparent", height = height, width = width-panel_padding)
          output$rose2 <- renderPlot({
            rose_plot(data = sdata$data, selected = if(is.null(input$nmark)){data_diff$selected}else{input$nmark},
                      title = paste0(sncell, " cells"),
                      maintitle = stitle, palette="RdYlGn")
          }, bg="transparent", height = height, width = width-panel_padding)
          output$diff <- renderPlot({box_plot(data = data_diff_,
                     selected = if(is.null(input$nmark)){data_diff$selected}else{input$nmark},
                     values = c("first"="#35978f", "second"="#bf812d"),
                     title = NULL, maintitle = "Differences: <span style = 'color:#35978f;'>**first**</span> vs <span style = 'color:#bf812d;'>**second**</span> selection",
                     palette="BrBG", colortitle = TRUE)
          }, bg="transparent", height = height, width = width-panel_padding)
        }
      }
    })


  })
}

## To be copied in the UI
# mod_panel_rose_ui("panel_rose_1")

## To be copied in the server
# mod_panel_rose_server("panel_rose_1")
