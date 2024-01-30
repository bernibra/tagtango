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
                 div(style="text-align: justify;",
                    span(
                      shinyWidgets::dropdownButton(
                        h5("Filter data:"),
                        uiOutput(ns("num_holder")),
                        shinyWidgets::materialSwitch(
                          inputId = ns("sort"),
                          label = "sort nodes by abundance",
                          value = FALSE,
                          status = "danger"
                        ),
                        circle = FALSE,
                        label = "Filter and sort",
                        #icon = icon("filter"),
                        size = "sm",
                        width = "300px",
                        #tooltip = shinyWidgets::tooltipOptions(title = "Click to filter the data!"),
                        status = "custom_filtering"
                      ),
                      " the data to identify interesting cell populations and ",
                      span(
                        shinyWidgets::dropdownButton(
                          p("Click on specific links to understand the cell composition of each label and on multiple links to compare their cell populations."),
                          circle = FALSE,
                          label = "select links",
                          size = "sm",
                          width = "300px",
                          status = "custom_filtering"
                        ),
                      ),
                      #span("click on specific links", style = "font-weight: 800; border-bottom: 1px solid #F4F1DE;"),
                      " to understand differences/similarities across annotations."
                    ),
                    br(),
                    br()
                  ),
                )
             )
    ),
    br(),
    fluidRow(
      fluidRow(
        uiOutput(ns("subtitle")),
        uiOutput(ns("grouping")),
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
               shiny::downloadButton(outputId = ns("download"), label = "Download", class = "custom"),
               shiny::actionButton(inputId = ns("restart"), label = "Restart", class = "custom")
        ),
      )
    ),
    fluidRow(
      mod_panel_decomposition_ui(ns("panel_decomposition_1")),
      mod_panel_rose_ui(ns("panel_rose_1"))
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
    values$norm <- data$norm
    values$rna_umap <- data$rna_umap
    values$adt_umap <- data$adt_umap
    values$p <- NULL
    values$max_value <- list(numVal = 1, numMin = 0, numMax = 100)
    values$code_fselection <- NULL
    values$code_sselection <- NULL

    left_color <- "#fbb4ae"
    right_color <- "#b3cde3"

    output$subtitle <- renderUI({tagList(
      div(class ="outerDiv_container", div(class = "outerDiv",
          column(4, align = "right",
                        h4(sentenceUp(gsub("[\\._-]", " ", data$left)), style = paste0("color: ", left_color,"; margin: 0px; padding: 0px; border: none;"))
          ),
          column(1, align="center",
               h3("VS", style ="color: #F4F1DE; margin: 0px; padding: 0px;")
          ),
          column(4, align = "left",
               h4(sentenceUp(gsub("[\\._-]", " ", data$right)), style = paste0("color:  ", right_color,"; margin: 0px; padding: 0px; border: none;"))
          )
      ))
    )})

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

    values$max_value$numMax <- max(values$network$links$value)

    numVal_d <- reactive({
      if(!is.null(input$num) && !is.na(input$num)){
        if(input$num < values$max_value$numMin) return(values$max_value$numMin)
        if(input$num > values$max_value$numMax) return(values$max_value$numMax)
        return(input$num)
      }else{
        return(values$max_value$numVal)
      }
    })

    if(!is.null(data$grouping_variable)){
      output$grouping <- renderUI({
        tagList(
          br(),
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
      numericInput(ns("num"), label = "Minimum number of cells per link:", value = numVal_d(), min = values$max_value$numMin, max = values$max_value$numMax-1) %>% smallInput(class = "form_filtering")
      })

    dataListen <- reactive({
      list(input$cells, input$num)
    })

    observeEvent(dataListen(), {
      shinyjs::disable(id = "num")

      values$network <- load_data(dat = values$dat, left = data$left, right = data$right,
                            rna_umap = values$rna_umap, adt_umap = values$adt_umap,
                            grouping_variable = data$grouping_variable, grouping_values = input$cells,
                            min_counts = numVal_d())
    })

    observeEvent(input$cells, {
      values$max_value$numMax <- max(values$network$links$value)
      shinyjs::enable(id = "num")
    })

    output$plot <- networkD3::renderSankeyNetwork({
      iterations <- ifelse(input$sort==F, 32, 0)
      san <- networkD3::sankeyNetwork(Links = values$network$links, Nodes = values$network$nodes,
                           Source = "source", Target = "target",
                           Value = "value", NodeID = "name",NodeGroup = "groups",
                           colourScale = paste0('d3.scaleOrdinal() .domain(["source", "target"]) .range(["', left_color,'", "', right_color,'"])'),
                           fontSize= 12, nodeWidth = 30, iterations = iterations)
      values$code_diagram <- paste0('
networkD3::sankeyNetwork(Links = dat$network$links, Nodes = dat$network$nodes,
                           Source = "source", Target = "target",
                           Value = "value", NodeID = "name",NodeGroup = "groups",
                           fontSize= 12, nodeWidth = 30, iterations = ', iterations, ')\n\n')

      htmlwidgets::onRender(san, stankeyNetwork_js(links = TRUE))
    })

    output$diagram <- renderUI({
      networkD3::sankeyNetworkOutput(ns("plot"), height = plotsize(max(c(length(unique(values$network$links[,1])), length(unique(values$network$links[,2]))))))
    })

    firstselection <- reactive({
      list(input$target1,input$source1, input$cells)
    })


    observeEvent(firstselection(), {
      if(!is.null(input$target1) | !is.null(input$source1)){

        width <- (input$width - (8/12) * 0.7 * input$width)/2
        height <- (input$width - (8/12) * 0.7 * input$width)/2

        values$code_fselection <- "## first selection\n\n"

        if(is.null(input$target1)){
          values$fselect <- values$network$dat$i==input$source1
          maintitle <- input$source1
          values$code_fselection <- paste0(values$code_fselection, "first_selection <- dat$network$dat$i==", rsym(input$source1), "\n")
          values$code_fselection <- paste0(values$code_fselection, "ftitle <- ", rsym(input$source1), "\n")
        }else if(is.null(input$source1)){
          values$fselect <- values$network$dat$j==input$target1
          maintitle <- input$target1
          values$code_fselection <- paste0(values$code_fselection, "first_selection <- dat$network$dat$j==", rsym(input$target1), "\n")
          values$code_fselection <- paste0(values$code_fselection, "ftitle <- ", rsym(input$target1), "\n")
        }else{
          values$fselect <- (values$network$dat$i==input$source1 & values$network$dat$j==input$target1)
          maintitle <- paste0(input$source1, " \u27A4 ", input$target1)
          values$code_fselection <- paste0(values$code_fselection, "first_selection <- (dat$network$dat$i==", rsym(input$source1), " & dat$network$dat$j==", rsym(input$target1), ")\n")
          values$code_fselection <- paste0(values$code_fselection, "ftitle <- paste0(", rsym(input$source1), ",' -> ',", rsym(input$target1),  ")\n")
        }

        values$ftitle <- maintitle

        if(any(values$fselect)){

          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = values$network$rna_umap, umap_adt = values$network$adt_umap,
                                         first_selection = values$fselect, height = height, width = width)

          mod_panel_rose_server("panel_rose_1", adt = values$norm, dat = values$network$dat,
                                ftitle = values$ftitle, fselection = values$fselect,
                                class = "top white", height = height, width = width, isRNA = data$data_type=="RNA")

        }else{
          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
          mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                                class = "top white")
          values$code_fselection <- NULL
        }
      }else{
        mod_panel_decomposition_server("panel_decomposition_1",
                                       umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
        mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                              class = "top white")
        values$code_fselection <- NULL
      }
    })


    secondselection <- reactive({
      list(input$target2,input$source2, input$cells)
    })

    observeEvent(secondselection(), {

      if(!is.null(input$target2) | !is.null(input$source2)){

        width <- (input$width - (8/12) * 0.7 * input$width)/2
        height <- (input$width - (8/12) * 0.7 * input$width)/2

        values$code_sselection <- "## second selection\n\n"

        if(is.null(input$target2)){
          values$sselect <- values$network$dat$i==input$source2
          maintitle <- input$source2
          values$code_sselection <- paste0(values$code_sselection, "second_selection <- dat$network$dat$i==", rsym(input$source2), "\n")
          values$code_sselection <- paste0(values$code_sselection, "stitle <- ", rsym(input$source2), "\n")
        }else if(is.null(input$source2)){
          values$sselect <- values$network$dat$j==input$target2
          maintitle <- input$target2
          values$code_sselection <- paste0(values$code_sselection, "second_selection <- dat$network$dat$j==", rsym(input$target2), "\n")
          values$code_sselection <- paste0(values$code_sselection, "stitle <- ", rsym(input$target2), "\n")
        }else{
          values$sselect <- (values$network$dat$i==input$source2 & values$network$dat$j==input$target2)
          maintitle <- paste0(input$source2, " \u27A4 ", input$target2)
          values$code_sselection <- paste0(values$code_sselection, "second_selection <- (dat$network$dat$i==", rsym(input$source2), " & dat$network$dat$j==", rsym(input$target2), ")\n")
          values$code_sselection <- paste0(values$code_sselection, "stitle <- paste0(", rsym(input$source2), ",' -> ',", rsym(input$target2),  ")\n")
        }

        values$stitle <- maintitle

        if(any(values$sselect)){

          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = values$network$rna_umap, umap_adt = values$network$adt_umap,
                                         first_selection = values$fselect, second_selection = values$sselect, height = height, width = width)

          mod_panel_rose_server("panel_rose_1", adt = values$norm, dat = values$network$dat,
                                stitle = values$stitle, ftitle = values$ftitle,
                                fselection = values$fselect, sselection = values$sselect,
                                class = "top white", height = height, width = width, isRNA = data$data_type=="RNA")

        }else{
          mod_panel_decomposition_server("panel_decomposition_1",
                                         umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
          mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                                class = "top white")
          values$code_sselection <- NULL

        }

      }else{
        mod_panel_decomposition_server("panel_decomposition_1",
                                       umap_rna = NULL, umap_adt = NULL, first_selection = NULL)
        mod_panel_rose_server("panel_rose_1", adt = NULL, dat = NULL, ftitle = NULL, stitle = NULL, fselection = NULL,
                              class = "top white")
        values$code_sselection <- NULL
      }
    })



    observeEvent(input$restart,{
      session$reload()
    })

    output$download <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("figures", ".R")
      },
      content = function(file) {
        # Write the R file that will be downloaded

        values$code_rose_figures <- generate_code_rose(values$norm, values$code_fselection, values$code_sselection, data$data_type)

        if(!is.null(values$network$rna_umap)){
          values$code_umap_rna <- generate_code_umap("dat$network$rna_umap", values$code_fselection, values$code_sselection, label = "first dimension")
        }else{
          values$code_umap_rna <- NULL
        }

        if(!is.null(values$network$adt_umap)){
          values$code_umap_adt <- generate_code_umap("dat$network$adt_umap", values$code_fselection, values$code_sselection, label = "second dimension")
        }else{
          values$code_umap_adt <- NULL
        }

        code <- c(paste0(data$code,
                       ", filter_variable = ", rsym(data$filter_variable),
                       ", filter_values = ", rsym(data$filter_values),
                       ", grouping_variable = ", rsym(data$grouping_variable),
                       ", grouping_values = ", rsym(input$cells),
                       ", min_counts = ", numVal_d(),
                       ")\n"),
                  "## Diagram",
                  values$code_diagram,
                  values$code_fselection,
                  values$code_sselection,
                  values$code_rose_figures,
                  values$code_umap_rna,
                  values$code_umap_adt
                  )
        writeLines(code, con = file, sep = "\n")
      }
    )


  })
}

## To be copied in the UI
# mod_sankeyNetwork_ui("sankeyNetwork_1")

## To be copied in the server
# mod_sankeyNetwork_server("sankeyNetwork_1")
