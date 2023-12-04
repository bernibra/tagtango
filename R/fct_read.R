#' readInput
#'
#' @description A utility read function to process the input data
#'
#' @return A list with the different components.
#'
#' @noRd
read_input <- function(filename) UseMethod("read_input", filename)

# Default read raw, guessing file type and loading data
read_input.default <- function(filename, ...){

  collimit <- 3000

  if(is.null(filename)){
    return(list(adt = NULL, norm = NULL, dat = NULL, ReadError = "No data"))
  }

  # File formatted as rds
  if (grepl(".rds$|.Rds$", filename)){
    return(check_dim(read_input.rds(filename), collimit = collimit))
  }

  # File formatted csv, tsv or txt
  if (grepl(".csv$|.tsv$|.txt$|.csv.gz$|.tsv.gz$|.txt.gz$", filename)){
    return(check_dim(read_input.csv(filename), collimit = collimit))
  }

  # # File formatted as h5
  # if (grepl(".h5$", filename)){
  #   return(read_input.h5(filename))
  # }

  return(list(adt = NULL, norm = NULL, dat = NULL, ReadError = "Wrong file type"))
}

# Read rds and consider it a csv
read_input.rds <- function(filename, ...){

  mat <- tryCatch({
    readRDS(filename)
  }, error = function(e) {
    NULL
  })

  if(is.null(mat)){
    return(list(adt = NULL, norm = NULL, dat = NULL, ReadError = "Wrong object type"))
  }

  if (class(mat)[1]=="SingleCellExperiment"){
    if("counts" %in% names(SummarizedExperiment::assays(mat))){
      adt <- SingleCellExperiment::counts(mat)
    }else{
      adt <- NULL
    }
    if("logcounts" %in% names(SummarizedExperiment::assays(mat))){
      norm <- SingleCellExperiment::logcounts(mat)
    }else{
      norm <- NULL
    }
    dat <- list(adt = adt, norm = norm, dat = SingleCellExperiment::colData(mat), ReadError = "Valid data")
  }else{
    dat <- tryCatch({
      list(adt = NULL, norm = NULL, dat = as.data.frame(mat), ReadError = "Valid data")
    }, error = function(e) {
      list(adt = NULL, norm = NULL, dat = NULL, ReadError = "Wrong file type")
    })
  }

  return(dat)

}

# Function turning a matrix type object to SingleCellExperiment class
read_input.csv <- function(filename, ...){

  # Load file as matrix using readr and tibble
  dat <- tryCatch({
    list(adt = NULL, norm = NULL, dat = as.data.frame(utils::read.csv(file = filename, header = T)), ReadError = "Valid data")
  }, error = function(e) {
    list(adt = NULL, norm = NULL, dat = NULL, ReadError = "Wrong file type")
  })

  return(dat)
}

check_dim <- function(x, collimit){
  if(is.null(x$dat)){
    return(x)
  }

  if(nrow(x$dat)==0){
    return(list(adt = NULL, norm = NULL, dat = NULL, ReadError = "Empty data"))
  }

  if(ncol(x$dat)>collimit){
    return(list(adt = NULL, norm = NULL, dat = NULL, ReadError = "Too many columns"))
  }
  return(x)
}
