#' Process data as the shiny application
#'
#' @description A utility function for reading a processing the data as `tagtango' does.
#' @param filename path to data file
#' @param data_type experiment used in the MultiAssayExperiment or data contained in logcounts in SingleCellExperiment
#' @param left annotation in the left of the diagram
#' @param right annotation in the right of the diagram
#' @param pc1_axis1 first axis of first dimension reduction space
#' @param pc1_axis2 second axis of first dimension reduction space
#' @param pc2_axis1 first axis of second dimension reduction space
#' @param pc2_axis2 second axis of second dimension reduction space
#' @param filter_variable filtering variable found in `dat'
#' @param filter_values values in `filter_variable' that need to be excluded
#' @param grouping_variable grouping variable found in `dat'
#' @param grouping_values values in `grouping_variable' that need to be filtered
#' @param min_counts minimum number of cells in a link for this to be displayed
#' @param dimension internal variable to determine whether the data is a data frame (=0), a low-dimension dataset (=1), or a high-dimension dataset (=2).
#' @param input_data object inputed directly.
#'
#' @return Returns a list containing two attributes: data and network. The first contains the data filtered according to the grouping and filtering values. The second, contains the data organized in a manner that is readible by the sankeyNetwork.
#' @export
process_data <- function(filename, data_type, left, right,
                         pc1_axis1 = NULL, pc1_axis2 = NULL,
                         pc2_axis1 = NULL, pc2_axis2 = NULL,
                         filter_variable = NULL, filter_values = NULL,
                         grouping_variable = NULL, grouping_values = NULL,
                         min_counts = NULL,
                         dimension = NULL,
                         input_data = NULL){

  if(is.null(filename)){
    if(is.null(input_data)){
      data <- read_input("test_data", run_test_data = TRUE)
    }else{
      data <-  read_input(filename = "argument", data = input_data)
      if(data$ReadError != "Valid data"){
        stop(data$ReadError)
      }
    }
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
      grouping_variable = grouping_variable,
      filter_variable = filter_variable,
      filter_values = filter_values,
      norm = sce,
      dimension = dimension,
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

  # Remove NAs and NaNs
  dat$dat[,dat$right] <- NAorNANcheck(dat$dat[,dat$right])
  dat$dat[,dat$left] <- NAorNANcheck(dat$dat[,dat$left])
  dat$dat[,dat$grouping_variable] <- NAorNANcheck(dat$dat[,dat$grouping_variable])
  dat$dat[,dat$filter_variable] <- NAorNANcheck(dat$dat[,dat$filter_variable])

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
    stop(dataproblem)
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
