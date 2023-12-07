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
mod_panel_rose_server <- function(id, adt, dat, selection, title = "title", class = "topleft white",
                                  height = 350, width = 350, top = "99%", left = "1%"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$panel <- renderUI({})

    shinyjs::hide("panel")

    output$panel <- renderUI({
      absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                    top = top, left = left, width = "auto", fixed=TRUE,
                    draggable = TRUE, height = "auto",
                    fluidRow(
                      column(12,
                             plotOutput(ns("rose"))
                      )
                    ),
                    uiOutput(ns("options_tab"))
      )
    })

    dataListen <- reactive({
      list(adt, selection)
    })

    observeEvent(dataListen(),{
      if(is.null(adt)){
        output$rose <- renderPlot({}, bg="transparent", height = height, width = width)
        output$options_tab <- renderUI({})
        shinyjs::hide("panel")
      }else{
        # browser()
        fexp <- adt[(rownames(adt) %in% dat$cells[selection]), ]
        ncell <- nrow(fexp)
        data <- find_markers(extra = 0, n = 15, mat = fexp)

        output$options_tab <- renderUI({
          tagList(
            fluidRow(column(5, offset = 7,
                            pickerInput(
                              inputId = ns("nmark"),
                              label = "",
                              choices = data$all,
                              options = pickerOptions(
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
          rose_plot(data = data$data, selected = input$nmark,
                    title = paste0(ncell, " cells"),
                    maintitle = title, palette="RdYlGn")
        }, bg="transparent", height = height, width = width)

        shinyjs::show("panel")
      }
    })


  })
}

## To be copied in the UI
# mod_panel_rose_ui("panel_rose_1")

## To be copied in the server
# mod_panel_rose_server("panel_rose_1")
