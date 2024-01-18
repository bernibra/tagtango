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
    otherproblem <- FALSE

    if(any(gsub("[[:punct:]]", " ", tolower(dat$data_type)) == c("adt", "antibody capture", "protein data", "antibody derived tags", "scadt"))){

      dat$norm <- tryCatch({
        t(as.matrix(SingleCellExperiment::logcounts(dat$sce)))
        }, error = function(e) {
        NULL
      })

      otherproblem <- ifelse(is.null(dat$norm), TRUE, otherproblem)
      if(!otherproblem){
        toomanycolumns <- ifelse(ncol(dat$norm)>2000, TRUE, toomanycolumns)
      }

    }else if(dat$data_type == "No expression data"){

      dat$norm <- NULL

    }else{

      dat$norm <- tryCatch({
        dge_rna_data(dat$sce, dat$left, dat$right, numberOFgenes = 5)
      }, error = function(e) {
        NULL
      })

      otherproblem <- ifelse(is.null(dat$norm), TRUE, otherproblem)

    }

    if(toomanycolumns){
      shinyalert::shinyalert(title = "Oups!", type = "warning", text = "The expression data is entered as ADT data, but the corresponding matrix, with more than 2000 columns, looks more like RNA data. Please specify the data type correctly.",
                             closeOnClickOutside = T, closeOnEsc = T,
                             animation = "pop", confirmButtonText = "Got it",
                             className = "warning_popup", confirmButtonCol = "#909097")
    }else if(otherproblem){
      shinyalert::shinyalert(title = "Oups!", type = "warning", text = "There is something odd regarding the expression data inputed. Please refer to the app's manual and README page for specifications on the input format.",
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
