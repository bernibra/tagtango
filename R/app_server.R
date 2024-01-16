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
    toomanycolumns <- FALSE

    if(any(gsub("[[:punct:]]", " ", tolower(dat$data_type)) == c("adt", "antibody capture", "protein data", "antibody derived tags"))){
      dat$norm <- t(as.matrix(SingleCellExperiment::logcounts(dat$sce)))
      toomanycolumns <- ifelse(ncol(dat$norm)>2000, TRUE, FALSE)
    }else if(dat$data_type == "No expression data"){
      dat$norm <- NULL
    }else{
      leftmat <- scran::scoreMarkers(dat$sce, SingleCellExperiment::colData(dat$sce)[,dat$left])
      leftgenes <- lapply(leftmat, function(x){
        ordered <- x[order(x$median.logFC.cohen,decreasing=TRUE),]
        ordered %>% head(5) %>% rownames()
      }) %>% unlist()
      names(leftgenes) <- NULL

      rightmat <- scran::scoreMarkers(dat$sce, SingleCellExperiment::colData(dat$sce)[,dat$right])
      rightgenes <- lapply(rightmat, function(x){
        ordered <- x[order(x$median.logFC.cohen,decreasing=TRUE),]
        ordered %>% head(5) %>% rownames()
      }) %>% unlist()
      names(rightgenes) <- NULL

      genes <- unique(c(leftgenes, rightgenes))
      dat$norm <- t(as.matrix(SingleCellExperiment::logcounts(dat$sce)[rownames(dat$sce) %in% genes,]))

    }

    if(toomanycolumns){
      shinyalert::shinyalert(title = "Oups!", type = "warning", text = "The expression data is entered as ADT data, but the corresponding matrix, with more than 2000 columns, looks more like RNA data. Please specify the data type correctly.",
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
