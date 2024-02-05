#' Process data as the shiny application
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
#' @param filter_variable filtering variable found in `dat`
#' @param filter_values values in `filter_variable` that need to be excluded
#' @param grouping_variable grouping variable found in `dat`
#' @param grouping_values values in `grouping_variable` that need to be filtered
#' @param min_counts minimum number of cells in a link for this to be displayed
#'
#' @return Returns a list containing two attributes: data and network. The first contains the data filtered according to the grouping and filtering values. The second, contains the data organized in a manner that is readible for the sankeyNetwork.
#' @export
process_data <- function(filename, data_type, left, right,
                         pc1_axis1 = NULL, pc1_axis2 = NULL,
                         pc2_axis1 = NULL, pc2_axis2 = NULL,
                         filter_variable = NULL, filter_values = NULL,
                         grouping_variable = NULL, grouping_values = NULL, min_counts = NULL){

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
      dge_rna_data(dat$sce, dat$dat, dat$left, dat$right, numberOFgenes = 5)
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

  if(!(is.null(filter_variable) || is.null(filter_values))){

    if(!(filter_variable %in% colnames(dat$dat))){
      stop("The filtering variable does not exist in the input data.frame or colData.")
    }

    if(!is.null(dat$rna_umap)){
      dat$rna_umap <- dat$rna_umap[!(dat$dat[,filter_variable] %in% filter_values),]
    }

    if(!is.null(dat$adt_umap)){
      dat$adt_umap <- dat$adt_umap[!(dat$dat[,filter_variable] %in% filter_values),]
    }

    dat$dat <- dat$dat %>% dplyr::filter(!(!!dplyr::sym(filter_variable) %in% !!filter_values))

  }

  network <- tryCatch({
                  load_data(dat = dat$dat, left = dat$left, right = dat$right,
                       rna_umap = dat$rna_umap, adt_umap = dat$adt_umap,
                       grouping_variable = grouping_variable, grouping_values = grouping_values,
                       min_counts = min_counts)
                }, error = function(e) {
                  NULL
                })

  if(is.null(network)){
    stop("Something is wrong with one of the following variables: `grouping_variable`, `grouping_values`, and `min_counts`")
  }

  return(
      list(data = dat,
           network = network)
    )
}
