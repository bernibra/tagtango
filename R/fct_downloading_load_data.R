#' process_data
#'
#' @description A utility function for reading a processing the data as `tagtango` does.
#' @param filename path to data file
#' @param data_type experiment used in the MultiAssayExperiment or data contained in logcounts in SingleCellExperiment
#' @param left annotation in the left of the diagram
#' @param right annotation in the right of the diagram
#' @param pc1_axis1 first axis of first dimension reduction space
#' @param pc1_axis2 second axis of first dimension reduction space
#' @param pc2_axis1 first axis of second dimension reduction space
#' @param pc2_axis2 second axis of second dimension reduction space
#'
#' @return The return value, if any, from executing the function.
#' @export
process_data <- function(filename, data_type, left, right,
                         pc1_axis1 = NULL, pc1_axis2 = NULL,
                         pc2_axis1 = NULL, pc2_axis2 = NULL){

  if(is.null(filename)){
    data <- read_input("test_data")
  }else{
    data <- read_input(filename)
    if(data$ReadError != "Valid data"){
      stop(data$ReadError)
    }
  }

  rna <- tryCatch({
    data.frame(V1 = data$dat[,pc1_axis1], V2 = data$dat[,pc1_axis2])
  }, error = function(e) {
    NULL
  })

  adt <- tryCatch({
    adt <- data.frame(V1 = data$dat[,pc2_axis1], V2 = data$dat[,pc2_axis2])
  }, error = function(e) {
    NULL
  })

  sce <- tryCatch({
    if(!is.null(data$mae)){data$mae[[data_type]]}else if(!is.null(data$sce)){data$sce}else{NULL}
  }, error = function(e) {
    NULL
  })

  dat <- tryCatch({
    list(
      dat = data$dat,
      left = left,
      right = right,
      sce = sce,
      rna_umap = rna,
      adt_umap = adt,
      data_type = data_type
    )
  }, error = function(e) {
    NULL
  })

  if(is.null(dat)){
    stop("There is something odd regarding the data inputed. Please refer to the app's manual and README page for specifications on the input format.")
  }

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
    stop("The expression data is entered as ADT data, but the corresponding matrix, with more than 2000 columns, looks more like RNA data. Please specify the data type correctly.")
  }

  if(otherproblem){
    stop("There is something odd regarding the expression data inputed. Please refer to the app's manual and README page for specifications on the input format.")
  }

  return(
      dat
    )
}
