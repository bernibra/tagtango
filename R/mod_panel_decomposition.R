#' panel_decomposition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_decomposition_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("panel"))
  )
}

#' panel_decomposition Server Functions
#'
#' @noRd
mod_panel_decomposition_server <- function(id, umap_rna, umap_adt, first_selection, second_selection = NULL,
                                           height = 350, width = 350, panel_padding = 20){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    if(is.null(second_selection)){
      labels = ifelse(first_selection, "selected", "other")
      values = c("other"="#D3D1C6", "selected"="#35978f")
    }else{
      labels = c("all other", "first", "second", "both")[1*first_selection + second_selection*2 +1]
      values = c("all other"="#D3D1C6", "first"="#35978f", "second"="#bf812d", "both" = "#808B5A")
    }

    dataListen <- reactive({
      list(umap_rna, umap_adt)
    })

    observeEvent(dataListen(),{
      if(!is.null(umap_rna) || !is.null(umap_adt)){
        output$panel <- renderUI({
          absolutePanel(id = "controls", class = "panel panel-default topleft white",
                        top = "99%", left = "99%", width = width, fixed=TRUE,
                        draggable = TRUE, height = "auto",
                        fluidRow(
                          uiOutput(ns("options_tab")),
                          column(12,
                                 plotOutput(ns("decomposition"),
                                            height = height-panel_padding,
                                            width = width-panel_padding)
                          )
                        )
          )
        })
        output$decomposition <- renderPlot({}, bg="transparent",
                                           height = height-panel_padding,
                                           width = width-panel_padding)
      }else{
        output$panel <- renderUI({})
      }

      if(is.null(umap_rna) && is.null(umap_adt)){
        output$decomposition <- renderPlot({}, bg="transparent",
                                           height = height-panel_padding,
                                           width = width-panel_padding)
        output$options_tab <- renderUI({})
      }else if(is.null(umap_rna)){
        output$options_tab <- renderUI({})
        output$decomposition <- renderPlot({
          return(plot_UMAP(data = umap_adt,
                    labels = labels,
                    values = values,
                    title = "ADT decomposition"
          ))}, bg="transparent", height = height-panel_padding, width = width-panel_padding)
      }else if(is.null(umap_adt)){
        output$options_tab <- renderUI({})
        output$decomposition <- renderPlot({
          return(plot_UMAP(data = umap_rna,
                           labels = labels,
                           values = values,
                           title = "RNA decomposition"
          ))}, bg="transparent",
          height = height-panel_padding,
          width = width-panel_padding)
        shinyjs::show("panel")
      }else{
        output$options_tab <- renderUI({tagList(
          column(12, align = "right",
                 shinyWidgets::radioGroupButtons(
                   inputId = ns("adtvsrna"),
                   choices = c("rna", "adt"),
                   status = "inwhite", selected = "rna"
                 )
          )
        )})
        output$decomposition <- renderPlot({
          d <- umap_rna
          title <- "RNA decomposition"
          if(ifelse(is.null(input$adtvsrna), FALSE, input$adtvsrna == "adt")){
            d <- umap_adt
            title <- "ADT decomposition"
          }
          return(plot_UMAP(data = d,
                           labels = labels,
                           values = values,
                           title = title
          ))}, bg="transparent",
          height = height-panel_padding,
          width = width-panel_padding)
      }
    })


  })
}

## To be copied in the UI
# mod_panel_decomposition_ui("panel_decomposition_1")

## To be copied in the server
# mod_panel_decomposition_server("panel_decomposition_1")
