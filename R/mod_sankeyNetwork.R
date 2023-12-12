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
    fluidRow(
      column(12,
             div(style="width:100%; justify-content: center;",
                 div(style="text-align: justify;", p("Interact with the diagram to visualize the different cell populations."))
             ))
    ),
    br(),
    fluidRow(
      fluidRow(
        uiOutput(ns("grouping")),
        # uiOutput(ns("decomposition")),
        column(5, align="right",
               uiOutput(ns("num_holder")),
               ),
        column(5,offset = 2, align= "left",
                     shinyWidgets::materialSwitch(
                       inputId = ns("sort"),
                       label = "sort by abundance",
                       value = FALSE,
                       status = "danger"
                     )
        )
      ),
      br(),
      fluidRow(
        column(8, offset = 2, align="center",
               uiOutput(ns("diagram"))
        )
      ),
      br(),
      fluidRow(
        column(8, offset = 2, align="right",
               shiny::downloadButton(outputId = ns("download"), label = "Download", class = "custom")
        ),
      )
    ),
    fluidRow(
      # uiOutput(ns("decomposition")),
      mod_panel_decomposition_ui(ns("panel_decomposition_1")),
      mod_panel_rose_ui(ns("panel_rose_1")),
      # mod_panel_rose_ui(ns("panel_rose_2")),
      # mod_panel_diff_ui(ns("panel_diff_1")),
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

    values$max_value <- max(values$network$links$value)

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

    output$num_holder <- renderUI({
      sliderInput(ns("num"), label = NULL, value = 1, min = 1, max = values$max_value-1, step = 1)
      })

    dataListen <- reactive({
      list(input$cells, input$num)
    })

    observeEvent(dataListen(), {
       values$network <- load_data(dat = values$dat, left = data$left, right = data$right,
                                  rna_umap = values$rna_umap, adt_umap = values$adt_umap,
                                  grouping_variable = data$grouping_variable, grouping_values = input$cells,
                                  min_counts = input$num)
       values$max_value <- max(values$network$links$value)
    })

    output$plot <- networkD3::renderSankeyNetwork({
      iterations <- ifelse(input$sort==F, 32, 0)
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

    firstselection <- reactive({
      list(input$target1,input$source1, input$cells)
    })


    observeEvent(firstselection(), {
      if(!is.null(input$target1) | !is.null(input$source1)){

        # height <- min(c(input$height*0.45, (input$width - (8/12) * 0.7 * input$width)/2))
        # width <- min(c(input$height*0.45, (input$width - (8/12) * 0.7 * input$width)/2))
        width <- (input$width - (8/12) * 0.7 * input$width)/2
        height <- (input$width - (8/12) * 0.7 * input$width)/2

        # print(c(width, height))

        if(is.null(input$target1)){
          values$fselect <- values$network$dat$i==input$source1
          maintitle <- input$source1
        }else if(is.null(input$source1)){
          values$fselect <- values$network$dat$j==input$target1
          maintitle <- input$target1
        }else{
          values$fselect <- (values$network$dat$i==input$source1 & values$network$dat$j==input$target1)
          maintitle <- paste0(input$source1, " \u27A4 ", input$target1)
        }

        values$ftitle <- maintitle

        if(any(values$fselect)){

          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = values$network$rna_umap, umap_adt = values$network$adt_umap,
                                         first_selection = values$fselect, height = height, width = width)

          mod_panel_rose_server("panel_rose_1", adt = values$norm, dat = values$network$dat,
                                ftitle = values$ftitle, fselection = values$fselect,
                                class = "top white", height = height, width = width)

        }else{
          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
          mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                                class = "top white")
        }
      }else{
        mod_panel_decomposition_server("panel_decomposition_1",
                                       umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
        mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                              class = "top white")
      }
    })


    secondselection <- reactive({
      list(input$target2,input$source2, input$cells)
    })

    observeEvent(secondselection(), {
      if(!is.null(input$target2) | !is.null(input$source2)){

        # height <- min(c(input$height*0.45, (input$width - (8/12) * 0.7 * input$width)/2))
        # width <- min(c(input$height*0.45, (input$width - (8/12) * 0.7 * input$width)/2))
        width <- (input$width - (8/12) * 0.7 * input$width)/2
        height <- (input$width - (8/12) * 0.7 * input$width)/2

        # print(c(width, height))

        if(is.null(input$target2)){
          values$sselect <- values$network$dat$i==input$source2
          maintitle <- input$source2
        }else if(is.null(input$source2)){
          values$sselect <- values$network$dat$j==input$target2
          maintitle <- input$target2
        }else{
          values$sselect <- (values$network$dat$i==input$source2 & values$network$dat$j==input$target2)
          maintitle <- paste0(input$source2, " \u27A4 ", input$target2)
        }

        values$stitle <- maintitle

        if(any(values$sselect)){

          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = values$network$rna_umap, umap_adt = values$network$adt_umap,
                                         first_selection = values$fselect, second_selection = values$sselect, height = height, width = width)

          mod_panel_rose_server("panel_rose_1", adt = values$norm, dat = values$network$dat,
                                stitle = values$stitle, ftitle = values$ftitle,
                                fselection = values$fselect, sselection = values$sselect,
                                class = "top white", height = height, width = width)

        }else{
          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
          mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                                class = "top white")

        }

      }else{
        mod_panel_decomposition_server("panel_decomposition_1",
                                       umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
        mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                              class = "top white")
      }
    })



  })
}

## To be copied in the UI
# mod_sankeyNetwork_ui("sankeyNetwork_1")

## To be copied in the server
# mod_sankeyNetwork_server("sankeyNetwork_1")
