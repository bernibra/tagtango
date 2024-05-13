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
                        tagList(
                          h5("Filter data:"),
                          p("Set the minimum number of cells per link:"),
                          column(12, align = "right", uiOutput(ns("num_holder"))),
                          p("Sort nodes by abundance: "),
                          column(12, align = "right",
                                    htmltools::tagAppendAttributes(
                                        shinyWidgets::materialSwitch(
                                          inputId = ns("sort"),
                                          label = NULL,
                                          value = FALSE,
                                          inline = TRUE,
                                          status = "danger"
                                        ),
                                        style = "text-align: initial;"
                                    )
                          ),
                          uiOutput(ns("density_section_holder"))
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
               shiny::actionButton(inputId = ns("restart"), label = "Restart", class = "custom"),
               shiny::actionButton(inputId = ns("stopapp"), label = "Stop", class = "custom")
        ),
      )
    ),
    fluidRow(
      mod_panel_decomposition_ui(ns("panel_decomposition_1")),
      mod_panel_rose_ui(ns("panel_rose_1")),
      mod_panel_df_ui(ns("panel_df_1"))
    )
  )
}

#' sankeyNetwork Server Functions
#'
#' @noRd
mod_sankeyNetwork_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Main data definition ----------------------------------------------------
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


    # Title found over the dendogram ------------------------------------------
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


    # Filter data if there are filtering parameters defined -------------------
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
    values$quantiles <- quantile(data$norm, probs = c(0.05, 0.95))
    values$density <- tryCatch({stats::density(as.numeric(data$norm))}, error = function(e) {NULL})

    # Dropdown menu with filtering parameters ---------------------------------
    # Defining the values for the cells per link parameter dynamically is actually a pain in the butt
    # This is the best possible solution I found
    numVal_d <- reactive({
      if(!is.null(input$num) && !is.na(input$num)){
        if(input$num < values$max_value$numMin) return(values$max_value$numMin)
        if(input$num > values$max_value$numMax) return(values$max_value$numMax)
        return(input$num)
      }else{
        return(values$max_value$numVal)
      }
    })

    output$num_holder <- renderUI({
      numericInput(ns("num"), label = NULL, value = numVal_d(), min = values$max_value$numMin, max = values$max_value$numMax-1) %>% smallInput(class = "form_filtering")
    })
    outputOptions(output, "num_holder", suspendWhenHidden=FALSE)


    # Add grouping values -----------------------------------------------------
    # When imputing the data if you enter a grouping variable like Main cell type, its values will be displayed on the top of the app
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

    # We also need to define a holder reactive value to avoid the app trying to process the data multiple times
    values$cell_group <- "All"
    observeEvent(input$cells, {
      values$cell_group <- input$cells
    }, ignoreInit = TRUE, priority = 100, ignoreNULL = TRUE)


    # Reactive value to figure out when to update the dendogram ---------------
    dataListen <- reactive({
      list(values$cell_group, input$num)
    })


    # Update data on call -----------------------------------------------------
    observeEvent(dataListen(), {
      shinyjs::disable(id = "num")
      values$first_selection <- NULL
      values$second_selection <- NULL
      values$network <- load_data(dat = values$dat, left = data$left, right = data$right,
                            rna_umap = values$rna_umap, adt_umap = values$adt_umap,
                            grouping_variable = data$grouping_variable, grouping_values = input$cells,
                            min_counts = numVal_d())

      values$quantiles <- quantile(values$norm, probs = c(0.05, 0.95))
      values$density <- tryCatch({stats::density(values$norm)}, error = function(e) {NULL})
    }, ignoreInit = TRUE, priority = 99, ignoreNULL = TRUE)

    observeEvent(dataListen(), {
      values$max_value$numMax <- max(values$network$links$value)
      shinyjs::enable(id = "num")
    }, ignoreInit = TRUE, priority = 98, ignoreNULL = TRUE)


    # Add dendogram holder-----------------------------------------------------------
    output$diagram <- renderUI({
      networkD3::sankeyNetworkOutput(ns("plot"), height = plotsize(max(c(length(unique(values$network$links[,1])), length(unique(values$network$links[,2]))))))
    })


    # Create dendogram with networkD3 -----------------------------------------
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


    # Reaction to selecting a link --------------------------------------------
    firstselection <- reactive({
      list(input$target1, input$source1, input$cells)
    })

    observeEvent(firstselection(), {
      if(!is.null(input$target1) | !is.null(input$source1)){

        values$width <- (input$width - (8/12) * 0.7 * input$width)/2
        values$height <- (input$width - (8/12) * 0.7 * input$width)/2

        values$code_fselection <- "quant <- quantile(dat$data$norm, probs = c(0.05, 0.95))\n\n## first selection\n\n"

        if(is.null(input$target1)){
          values$first_selection <- values$network$dat$i==input$source1
          maintitle <- input$source1
          values$code_fselection <- paste0(values$code_fselection, "first_selection <- dat$network$dat$i==", rsym(input$source1), "\n")
          values$code_fselection <- paste0(values$code_fselection, "ftitle <- ", rsym(input$source1), "\n")
        }else if(is.null(input$source1)){
          values$first_selection <- values$network$dat$j==input$target1
          maintitle <- input$target1
          values$code_fselection <- paste0(values$code_fselection, "first_selection <- dat$network$dat$j==", rsym(input$target1), "\n")
          values$code_fselection <- paste0(values$code_fselection, "ftitle <- ", rsym(input$target1), "\n")
        }else{
          values$first_selection <- (values$network$dat$i==input$source1 & values$network$dat$j==input$target1)
          maintitle <- paste0(input$source1, " \u27A4 ", input$target1)
          values$code_fselection <- paste0(values$code_fselection, "first_selection <- (dat$network$dat$i==", rsym(input$source1), " & dat$network$dat$j==", rsym(input$target1), ")\n")
          values$code_fselection <- paste0(values$code_fselection, "ftitle <- paste0(", rsym(input$source1), ",' -> ',", rsym(input$target1),  ")\n")
        }

        values$ftitle <- maintitle

        if(!any(values$first_selection)){
          values$first_selection <- NULL
        }
      }else{
        values$first_selection <- NULL
        values$second_selection <- NULL
        values$code_fselection <- NULL
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE, priority = 0)

    secondselection <- reactive({
      list(input$target2,input$source2, input$cells)
    })


    # Reaction to selecting a second link -------------------------------------
    observeEvent(secondselection(), {
      if(!is.null(input$target2) | !is.null(input$source2)){

        values$width <- (input$width - (8/12) * 0.7 * input$width)/2
        values$height <- (input$width - (8/12) * 0.7 * input$width)/2

        values$code_sselection <- "## second selection\n\n"

        if(is.null(input$target2)){
          values$second_selection <- values$network$dat$i==input$source2
          maintitle <- input$source2
          values$code_sselection <- paste0(values$code_sselection, "second_selection <- dat$network$dat$i==", rsym(input$source2), "\n")
          values$code_sselection <- paste0(values$code_sselection, "stitle <- ", rsym(input$source2), "\n")
        }else if(is.null(input$source2)){
          values$second_selection <- values$network$dat$j==input$target2
          maintitle <- input$target2
          values$code_sselection <- paste0(values$code_sselection, "second_selection <- dat$network$dat$j==", rsym(input$target2), "\n")
          values$code_sselection <- paste0(values$code_sselection, "stitle <- ", rsym(input$target2), "\n")
        }else{
          values$second_selection <- (values$network$dat$i==input$source2 & values$network$dat$j==input$target2)
          maintitle <- paste0(input$source2, " \u27A4 ", input$target2)
          values$code_sselection <- paste0(values$code_sselection, "second_selection <- (dat$network$dat$i==", rsym(input$source2), " & dat$network$dat$j==", rsym(input$target2), ")\n")
          values$code_sselection <- paste0(values$code_sselection, "stitle <- paste0(", rsym(input$source2), ",' -> ',", rsym(input$target2),  ")\n")
        }

        values$stitle <- maintitle

        if(!any(values$second_selection)){
          values$first_selection <- NULL
          values$second_selection <- NULL
          values$code_sselection <- NULL
        }
      }else{
        values$first_selection <- NULL
        values$second_selection <- NULL
        values$code_sselection <- NULL
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE, priority = -1)

    # Absolute panel on the right ---------------------------------------------
    # This is the UMAP plots
    if(!is.null(values$network$rna_umap) || !is.null(values$network$adt_umap)){
      mod_panel_decomposition_server("panel_decomposition_1", values = values, rnatitle = data$RNAtitle, adttitle = data$ADTtitle)
    }

    # Absolute panel on the left ----------------------------------------------
    # These are the rose plots
    if(is.null(values$norm)){
      # Actual absolute panel on the left with the confusion matrix plot and metrics
      mod_panel_df_server("panel_df_1", values = values,
                            class = "top white")
    }else{
      # Actual absolute panel on the left with the rose plot
      mod_panel_rose_server("panel_rose_1", values = values,
                            class = "top white")
    }

    if(is.null(values$density)){
      output$density_holder <- renderPlot({})
      values$valley_position <- NULL
    }else{
      # Define UI for the additional filtering based on density
      output$density_section_holder <- renderUI({tagList(
        p("Set the valley position for color style:"),
        column(12, align = "right", plotOutput(ns("density_holder"), width = "95%", height = "200px", click = ns("valley_position")))
      )})

      # Density plot in the filtering drowpdown menu
      output$density_holder <- renderPlot(expr =
                                            ggplot(data=data.frame(x = values$density$x, y = values$density$y), aes(x = x, y=y)) +
                                            geom_line() +
                                            geom_vline(xintercept = values$valley_position, color = "#F4BA02") +
                                            xlab("Expression") +
                                            ggtitle(label = "", subtitle = paste0("Valley position: ", as.character(ifelse(is.null(values$valley_position), "none", round(values$valley_position, digits = 3))))) +
                                            theme_bw() +
                                            theme(text = element_text(colour = "#3D405B", size = 14),
                                                  axis.title.y = element_blank(),
                                                  axis.text.y = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  panel.grid = element_blank(),
                                                  panel.border = element_rect(color = "#3D405B", linewidth = 1),
                                                  plot.title = element_blank(),
                                                  plot.subtitle = element_text(hjust=1, vjust=0.5, size = 11)
                                            )
      )

      # Valley position in the density plot above
      observeEvent(input$valley_position$x, {
        values$valley_position <- input$valley_position$x
      }, ignoreInit = TRUE, ignoreNULL = TRUE, priority = 97)

    }


    # Restart app on click ----------------------------------------------------
    observeEvent(input$restart,{
      session$reload()
    })

    # Stop app on click ----------------------------------------------------
    observeEvent(input$stopapp,{
      shiny::stopApp()
    })

    # Download button ---------------------------------------------------------
    output$download <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("figures", ".R")
      },
      content = function(file) {
        # Write the R file that will be downloaded
        values$code_rose_figures <- generate_code_rose(values$norm, values$code_fselection, values$code_sselection, data$data_type)

        if(!is.null(values$network$rna_umap)){
          values$code_umap_rna <- generate_code_umap("dat$network$rna_umap", values$code_fselection, values$code_sselection, xlabel = data$RNAtitle$x, ylabel = data$RNAtitle$y, label = "first projection")
        }else{
          values$code_umap_rna <- NULL
        }

        if(!is.null(values$network$adt_umap)){
          values$code_umap_adt <- generate_code_umap("dat$network$adt_umap", values$code_fselection, values$code_sselection, xlabel = data$ADTtitle$x, ylabel = data$ADTtitle$y, label = "second projection")
        }else{
          values$code_umap_adt <- NULL
        }

        code <- c(paste0(data$code,
                         data$codebit,
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
