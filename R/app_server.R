#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # defining reactive values
  values <- reactiveValues()

  # Landing page
  output$content <- renderUI(mod_input_data_ui("input_data_1"))

  # Landing page logic
  data <- mod_input_data_server("input_data_1")
  shinyjs::disable("load")

  observeEvent(input$load,{
    dat <- data()

    if(any(gsub("[[:punct:]]", " ", tolower(dat$data_type)) == c("adt", "antibody capture", "protein data", "antibody derived tags", "scadt"))){

      dat$norm <- tryCatch({
        t(as.matrix(SingleCellExperiment::logcounts(dat$sce)))
        }, error = function(e) {
        NULL
      })

      dataproblem <- run_basic_checks(norm = dat$norm, dat = dat$dat, maxcol = 2000)

      data_type <- "ADT"

    }else if(dat$data_type == "No expression data"){

      dat$norm <- NULL
      data_type <- "No expression data"
      dataproblem <- NULL

    }else{

      dat$norm <- tryCatch({
        dge_rna_data(dat$sce, dat$dat, dat$left, dat$right, numberOFgenes = ifelse(nrow(dat$sce)>=10, 10, nrow(dat$sce)))
      }, error = function(e) {
        NULL
      })

      dataproblem <- run_basic_checks(norm = dat$norm, dat = dat$dat)

      data_type <- "RNA"
    }

    dat$data_type <- data_type

    if(!is.null(dataproblem)){
      shinyalert::shinyalert(title = "Oups!", type = "warning", text = dataproblem,
                             closeOnClickOutside = T, closeOnEsc = T,
                             animation = "pop", confirmButtonText = "Got it",
                             className = "warning_popup", confirmButtonCol = "#909097")
    }else{
      shinyjs::disable("load")
      shinyjs::hide("load")
      output$content <- renderUI(mod_sankeyNetwork_ui("sankeyNetwork_1"))
      mod_sankeyNetwork_server("sankeyNetwork_1", dat)
    }
  })

}
