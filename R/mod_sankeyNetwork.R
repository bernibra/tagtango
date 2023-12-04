#' sankeyNetwork UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sankeyNetwork_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    fluidRow(
      fluidRow(
        uiOutput(ns("grouping")),
        column(4,offset = 6, align= "left",
               column(12,
                      column(6,
                             shinyWidgets::awesomeCheckbox(
                               inputId = ns("sort"),
                               label = "sort",
                               value = TRUE,
                               status = "danger"
                             )
                      ),
               )
        )
      ),
      br(),
      fluidRow(
        column(8, offset = 2, align="center",
               uiOutput(ns("diagram")),
        )
      ),
      br(),
      fluidRow(
        column(8, offset = 2, align="right",
               shiny::downloadButton(outputId = ns("download"), label = "Download", class = "custom")
        ),
      )
    )
  )
}

#' sankeyNetwork Server Functions
#'
#' @noRd
mod_sankeyNetwork_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()
    values$dat <- data$dat
    values$adt <- data$adt
    values$norm <- data$norm
    values$rna_umap <- data$rna_umap
    values$adt_umap <- data$adt_umap
    values$p <- NULL

    # filter dat
    if(!is.null(data$filter_variable)){

      if(!is.null(values$rna_umap)){
        values$rna_umap <- values$rna_umap[!(values$dat[,data$filter_variable] %in% data$filter_values),]
      }

      if(!is.null(values$adt_umap)){
        values$adt_umap <- values$adt_umap[!(values$dat[,data$filter_variable] %in% data$filter_values),]
      }

      values$dat <- values$dat %>% dplyr::filter(!(!!dplyr::sym(data$filter_variable) %in% !!data$filter_values))

    }

    values$network <- load_data(dat = values$dat, left = data$left, right = data$right,
                                rna_umap = values$rna_umap, adt_umap = values$adt_umap)

    if(!is.null(data$grouping_variable)){
      output$grouping <- renderUI({
        tagList(
          column(12, align = "center",
                 shinyWidgets::radioGroupButtons(
                   inputId = ns("cells"),
                   choices = c("All", as.character(unique(data$dat[,data$grouping_variable]))),
                   status = "custom",
                   selected = "All"
                 ),
          ),
        )
      })
    }


    dataListen <- reactive({
      list(input$cells)
    })

    observeEvent(dataListen(), {
      values$network <- load_data(dat = values$dat, left = data$left, right = data$right,
                                  rna_umap = values$rna_umap, adt_umap = values$adt_umap,
                                  grouping_variable = data$grouping_variable, grouping_values = input$cells)
    })

    output$plot <- networkD3::renderSankeyNetwork({
      iterations <- ifelse(input$sort, 32, 0)
      san <- networkD3::sankeyNetwork(Links = values$network$links, Nodes = values$network$nodes,
                           Source = "source", Target = "target",
                           Value = "value", NodeID = "name",NodeGroup = "groups",
                           colourScale = 'd3.scaleOrdinal() .domain(["source", "target"]) .range(["#D3D1C6", "#909097"])',
                           fontSize= 12, nodeWidth = 30, iterations = iterations)

      htmlwidgets::onRender(san, stankeyNetwork_js())
    })

    output$diagram <- renderUI({
      networkD3::sankeyNetworkOutput(ns("plot"), height = plotsize(max(c(length(unique(values$network$links[,1])), length(unique(values$network$links[,2]))))))
    })

  })
}

## To be copied in the UI
# mod_sankeyNetwork_ui("sankeyNetwork_1")

## To be copied in the server
# mod_sankeyNetwork_server("sankeyNetwork_1")
