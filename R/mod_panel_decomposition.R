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
mod_panel_decomposition_server <- function(id, values, panel_padding = 20, rnatitle = NULL, adttitle = NULL){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    dataListen <- reactive({
      list(values$first_selection, values$second_selection)
    })

    observeEvent(dataListen(),{
      # print("umap")
      if(is.null(values$first_selection)){
        glabels = NULL
        gvalues = NULL
      }else if(is.null(values$second_selection)){
        glabels = ifelse(values$first_selection, "selected", "other")
        gvalues = c("other"="#D3D1C6", "selected"="#35978f")
      }else{
        glabels = c("all other", "first", "second", "both")[1*values$first_selection + values$second_selection*2 +1]
        gvalues = c("all other"="#D3D1C6", "first"="#35978f", "second"="#bf812d", "both" = "#808B5A")
      }


      if(!is.null(glabels)){
        output$panel <- renderUI({
          absolutePanel(id = "controls", class = "panel panel-default topleft white",
                        top = "98%", left = "99%", width = values$width, fixed=TRUE,
                        draggable = F, height = "auto",
                        fluidRow(
                          uiOutput(ns("options_tab")),
                          column(12,
                                 plotOutput(ns("decomposition"),
                                            height = values$height-panel_padding,
                                            width = values$width-panel_padding)
                          ),
                        ),
                        div(style = "position: relative;",
                            shiny::actionButton(ns("UMAPinfo"), label = "i",
                                                style = "bottom: -2px; left:-2px;",
                                                class = "btn btn-custom bttn-panel")
                        )
          )
        })
        output$decomposition <- renderPlot({}, bg="transparent",
                                           height = values$height-panel_padding,
                                           width = values$width-panel_padding)
      }else{
        output$panel <- renderUI({})
      }

      if(is.null(glabels)){
        output$decomposition <- renderPlot({}, bg="transparent",
                                           height = values$height-panel_padding,
                                           width = values$width-panel_padding)
        output$options_tab <- renderUI({})
      }else if(is.null(values$network$rna_umap)){
        output$options_tab <- renderUI({})
        output$decomposition <- renderPlot({
          return(scatter_plot(data = values$network$adt_umap,
                    labels = glabels,
                    values = gvalues,
                    title = adttitle$title,
                    xlabel = adttitle$x,
                    ylabel = adttitle$y
          ))}, bg="transparent", height = values$height-panel_padding, width = values$width-panel_padding)
      }else if(is.null(values$network$adt_umap)){
        output$options_tab <- renderUI({})
        output$decomposition <- renderPlot({
          return(scatter_plot(data = values$network$rna_umap,
                           labels = glabels,
                           values = gvalues,
                           title = rnatitle$title,
                           xlabel = rnatitle$x,
                           ylabel = rnatitle$y
          ))}, bg="transparent",
          height = values$height-panel_padding,
          width = values$width-panel_padding)
        shinyjs::show("panel")
      }else{
        output$options_tab <- renderUI({tagList(
          column(12, align = "right",
                 shinyWidgets::radioGroupButtons(
                   inputId = ns("adtvsrna"),
                   choices = c(rnatitle$tab, adttitle$tab),
                   status = "inwhite", selected = rnatitle$tab
                 )
          )
        )})
        output$decomposition <- renderPlot({
          d <- values$network$rna_umap
          title <- rnatitle$title
          xlabel <- rnatitle$x
          ylabel <- rnatitle$y
          if(ifelse(is.null(input$adtvsrna), FALSE, input$adtvsrna == adttitle$tab)){
            d <- values$network$adt_umap
            title <- adttitle$title
            xlabel <- adttitle$x
            ylabel <- adttitle$y
          }
          return(scatter_plot(data = d,
                           labels = glabels,
                           values = gvalues,
                           title = title,
                           xlabel = xlabel,
                           ylabel = ylabel
          ))}, bg="transparent",
          height = values$height-panel_padding,
          width = values$width-panel_padding)
      }
    }, ignoreInit = TRUE, priority = 10, ignoreNULL = TRUE)

    observeEvent(input$UMAPinfo, {
      shinyalert::shinyalert(html=T, text = tagList(
        h4("Scatter plot", style = "text-align: justify;"),
        p("The scatter plot displays the position of the cells/spots/rows/elements within the selection along the chosen axes/dimensions. For example, if the axes selected are the principal components for the expression of a set of markers, the scatter plot will showcase where each selection is positioned along those axes.", style = "text-align: justify;")
      ),
      closeOnClickOutside = T, closeOnEsc = T, animation = "pop",type = "info",
      confirmButtonText = "Got it", className = "information_popup", confirmButtonCol = "#909097")
    })

  })
}

## To be copied in the UI
# mod_panel_decomposition_ui("panel_decomposition_1")

## To be copied in the server
# mod_panel_decomposition_server("panel_decomposition_1")
