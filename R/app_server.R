#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # defining reactive values
  values <- reactiveValues()

  browser()

  # Landing page
  output$content <- renderUI(mod_input_data_ui("input_data_1"))

  # Landing page logic
  data <- mod_input_data_server("input_data_1")
  shinyjs::disable("load")


  observeEvent(input$load,{
    dat <- data()

    # Filter data if there are filtering parameters defined -------------------
    if(!is.null(dat$filter_variable)){

      if(!is.null(dat$rna_umap)){
        dat$rna_umap <- dat$rna_umap[!(dat$dat[,dat$filter_variable] %in% dat$filter_values),]
      }

      if(!is.null(dat$adt_umap)){
        dat$adt_umap <- dat$adt_umap[!(dat$dat[,dat$filter_variable] %in% dat$filter_values),]
      }

      dat$norm <- dat$norm[, !(dat$dat[,dat$filter_variable] %in% dat$filter_values)]
      dat$dat <- dat$dat %>% dplyr::filter(!(!!dplyr::sym(dat$filter_variable) %in% !!dat$filter_values))
    }

    # Remove unnecessary information
    if (is.null(dat$grouping_variable)){
      dat$dat <- dat$dat %>% dplyr::select(!!dplyr::sym(dat$left), !!dplyr::sym(dat$right))
    }else{
      dat$dat <- dat$dat %>% dplyr::select(!!dplyr::sym(dat$grouping_variable), !!dplyr::sym(dat$left), !!dplyr::sym(dat$right))
    }

    # Work on the expression data to transform accordingly
    if(dat$dimension == 0){

      dat$norm <- NULL
      data_type <- "No expression data"
      dataproblem <- NULL

    }else if(dat$dimension == 1){

      dat$norm <- tryCatch({
        Matrix::t(SingleCellExperiment::logcounts(dat$norm))
        }, error = function(e) {
        NULL
      })

      dataproblem <- run_basic_checks(norm = dat$norm, dat = dat$dat, maxcol = 2000)

      data_type <- "ADT"

    }else{

      genes <- tryCatch({
        dge_rna_data(dat$norm, dat$dat, dat$left, dat$right, numberOFgenes = ifelse(nrow(dat$norm)>=10, 10, nrow(dat$norm)))
      }, error = function(e) {
        NULL
      })

      dat$norm <- dat$norm[rownames(dat$norm) %in% genes,]
      dat$norm <- tryCatch({Matrix::t(SingleCellExperiment::logcounts(dat$norm))}, error = function(e) {NULL})

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
