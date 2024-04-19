#' panel_df UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_df_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("panel"))
  )
}

#' panel_df Server Functions
#'
#' @noRd
mod_panel_df_server <- function(id, values,
                                class = "topleft white", panel_padding = 20,
                                top = "98%", left = "1%"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataListen <- reactive({
      list(values$first_selection, values$second_selection)
    })

    observeEvent(dataListen(),{

      if(is.null(values$first_selection)){
        output$confusion <- renderUI({})
        output$panel <- renderUI({})
      }else if(is.null(values$second_selection)) {

        output$panel <- renderUI({
          absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                        top = top, left = left, width = values$width, fixed=TRUE,
                        draggable = F, height = "auto",
                        fluidRow(column(6, h5("Overall AMI", align = "left", style="color:#3D405B; border: none;")),
                                 column(6,h5(textOutput(ns("NMI")), align = "right", style="color:#3D405B; border: none;"))),
                        fluidRow(column(6, h5("Coherence", align = "left", style="color:#3D405B; border: none;")),
                                 column(6,h5(textOutput(ns("coherence")), align = "right", style="color:#3D405B; border: none;"))),
                        br(),
                          fluidRow(column(12, p("The overall Adjusted Mutual Information (AMI) is calculated following the 'aricode' R package, scaling the results between 0 (random) and 1 (identical). The coherence reflects the extend to which the link or nodes selected show agreement across annotations ([see details]()).",
                                        align = "justify", style="color:#3D405B"))),

          )
        })

        select_cells <- values$network$dat[(values$network$dat$j %in% unique(values$network$dat$j[values$first_selection]) | values$network$dat$i %in% unique(values$network$dat$i[values$first_selection])),]

        output$NMI <- renderText({
          round(aricode::AMI(values$network$dat$i, values$network$dat$j), digits = 4)
        })

        output$coherence <- renderText({
          round(coherence(select_cells %>% dplyr::select(i, j)), digits = 4)
        })


      }else{

        output$panel <- renderUI({
          absolutePanel(id = "controls", class = paste0("panel panel-default ", class),
                        top = top, left = left, width = values$width, fixed=TRUE,
                        draggable = F, height = "auto",
                        fluidRow(column(6, h5("Overall AMI", align = "left", style="color:#3D405B; border: none;")),
                                 column(6,h5(textOutput(ns("NMI")), align = "right", style="color:#3D405B; border: none;"))),
                        fluidRow(column(6, h5("1st selection coherence", align = "left", style="color:#3D405B; border: none;")),
                                 column(6,h5(textOutput(ns("fcoherence")), align = "right", style="color:#3D405B; border: none;"))),
                        fluidRow(column(6, h5("2nd selection coherence", align = "left", style="color:#3D405B; border: none;")),
                                 column(6,h5(textOutput(ns("scoherence")), align = "right", style="color:#3D405B; border: none;"))),
                        fluidRow(column(6, h5("Overlap", align = "left", style="color:#3D405B; border: none;")),
                                 column(6,h5(textOutput(ns("overlap")), align = "right", style="color:#3D405B; border: none;"))),
                        br(),
                        fluidRow(column(12, p("The overall Adjusted Mutual Information (AMI) is calculated following the 'aricode' R package, scaling the results between 0 (random) and 1 (identical). The Contribution to AMI is calculated by taking the subgraph defined by the interacting partners of those nodes selected.",
                                              align = "justify", style="color:#3D405B"))),

          )
        })

        fselect_cells <- values$network$dat[((values$network$dat$j %in% unique(values$network$dat$j[values$first_selection])) | (values$network$dat$i %in% unique(values$network$dat$i[values$first_selection]))),]
        sselect_cells <- values$network$dat[((values$network$dat$j %in% unique(values$network$dat$j[values$second_selection])) | (values$network$dat$i %in% unique(values$network$dat$i[values$second_selection]))),]

        output$NMI <- renderText({
          round(aricode::AMI(values$network$dat$i, values$network$dat$j), digits = 4)
        })
        output$fcoherence <- renderText({
          round(coherence(fselect_cells %>% dplyr::select(i, j)), digits = 4)
        })
        output$scoherence <- renderText({
          round(coherence(sselect_cells %>% dplyr::select(i, j)), digits = 4)
        })


        output$overlap <- renderText({
          paste(sum(values$first_selection & values$second_selection), "cells", sep=" ")
        })

      }

    }, ignoreInit = TRUE, priority = 9, ignoreNULL = TRUE)

  })
}

## To be copied in the UI
# mod_panel_df_ui("panel_df_1")

## To be copied in the server
# mod_panel_df_server("panel_df_1")
