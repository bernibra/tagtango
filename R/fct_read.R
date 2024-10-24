#' readInput
#'
#' @description A utility read function to process the input data
#'
#' @return A list with the different components.
#'
#' @noRd
read_input <- function(filename, run_test_data = FALSE, data = NULL, ...) UseMethod("read_input")

# Default read raw, guessing file type and loading data
#'
#' @noRd
read_input.default <- function(filename, run_test_data = FALSE, data = NULL, ...){

  collimit <- 3000

  if(is.na(filename)){
    return(list(sce = NULL,
                mae = NULL,
                dat = NULL, ReadError = "Error loading the data. Refer to the app's manual and README page for specifications on the input format.",
                warning = ""))
  }

  if(is.null(filename)){
    return(list(sce = NULL,
                mae = NULL,
                dat = NULL, ReadError = "No data",
                warning = ""))
  }

  if(run_test_data){
    return(read_input.test())
  }

  if(!is.null(data)){
    return(check_dim(read_input.object(data, typ = "object"), collimit = collimit))
  }

  # File formatted as rds
  if (grepl(".rds$|.Rds$", filename)){
    return(check_dim(read_input.rds(filename), collimit = collimit))
  }

  # File formatted csv, tsv or txt
  if (grepl(".csv$|.tsv$|.txt$", filename)){
    return(check_dim(read_input.csv(filename), collimit = collimit))
  }

  return(list(
    sce = NULL,
    mae = NULL,
    dat = NULL, ReadError = "Wrong file type",
    warning = ""))
}

# Read tesdata
#'
#' @noRd
read_input.test <- function(){
  utils::data("test_data")

  if(class(test_data)[1]=="SingleCellExperiment"){

    dat <- as.data.frame(SingleCellExperiment::colData(test_data))
    if(length(SingleCellExperiment::reducedDimNames(test_data))!=0){
      for(i in SingleCellExperiment::reducedDimNames(test_data)){
        d <- as.data.frame(SingleCellExperiment::reducedDim(test_data, type = i)[,1:2])
        colnames(d) <- paste0(i, c("_first_axis", "_second_axis"))
        dat <- cbind(dat, d)
      }
    }

    dat <- list(
      sce = test_data, mae = NULL,
      dat = dat, ReadError = "Valid data",
      warning = "")

  }else{
    coldat <- as.data.frame(MultiAssayExperiment::colData(test_data))
    for (j in names(test_data)){
      sce <- MultiAssayExperiment::experiments(test_data)[[j]]
      if(length(SingleCellExperiment::reducedDimNames(sce))!=0){
        for(i in SingleCellExperiment::reducedDimNames(sce)){
          d <- as.data.frame(SingleCellExperiment::reducedDim(sce, type = i)[,1:2])
          colnames(d) <- paste0(j, "_", i, c("_first", "_second"))
          coldat <- cbind(coldat, d)
        }
      }
    }

    dat <- list(
      mae = test_data,
      sce = NULL,
      dat = coldat,
      ReadError = "Valid data",
      warning = ""
    )
  }

  return(dat)
}

# Read object and check if it is possible to be processed
#'
#' @noRd
read_input.object <- function(mat, typ, ...){

  if(is.null(mat)){
    return(list(
      sce = NULL,
      mae = NULL,
      dat = NULL, ReadError = paste("Wrong", typ , "type", sep = " "),
      warning = ""))
  }

  if(class(mat)[1]=="SingleCellExperiment"){

    sce <- mat

    if(!("logcounts" %in% SummarizedExperiment::assayNames(sce))){
      assaynames <- SummarizedExperiment::assayNames(sce)
      assaypicked <- assaynames[length(assaynames)]
      SummarizedExperiment::assayNames(sce)[length(assaynames)] <- "logcounts"
      warning <- paste0("There is no assay in the SingleCellExperiment named 'logcounts'. Picking '",assaypicked,"' by default")
    }else{
      warning <- ""
    }

    dat <- as.data.frame(SingleCellExperiment::colData(mat))
    if(length(SingleCellExperiment::reducedDimNames(mat))!=0){
      for(i in SingleCellExperiment::reducedDimNames(mat)){
        d <- as.data.frame(SingleCellExperiment::reducedDim(mat, type = i)[,1:2])
        colnames(d) <- paste0(i, c("_first", "_second"))
        dat <- cbind(dat, d)
      }
    }

    dat <- list(
      sce = sce, mae = NULL,
      dat = dat, ReadError = "Valid data",
      warning = warning)

  }else if (class(mat)[1]=="MultiAssayExperiment"){

    mae <- mat

    dat <- as.data.frame(MultiAssayExperiment::colData(mae))

    warning <- ""

    for (j in names(mae)){

      sce <- MultiAssayExperiment::experiments(mae)[[j]]

      if(!("logcounts" %in% SummarizedExperiment::assayNames(sce))){
        assaynames <- SummarizedExperiment::assayNames(sce)
        assaypicked <- assaynames[length(assaynames)]
        SummarizedExperiment::assayNames(MultiAssayExperiment::experiments(mae)[[j]])[length(assaynames)] <- "logcounts"
        warning <- paste0(warning, "For the '",j,"' experiment, there is no assay named 'logcounts'; picking '",assaypicked,"' by default. ")
      }

      if(length(SingleCellExperiment::reducedDimNames(sce))!=0){
        for(i in SingleCellExperiment::reducedDimNames(sce)){
          d <- as.data.frame(SingleCellExperiment::reducedDim(sce, type = i)[,1:2])
          colnames(d) <- paste0(j, "_", i, c("_first", "_second"))
          dat <- cbind(dat, d)
        }
      }
    }

    dat <- list(
      sce = NULL,
      mae = mae,
      dat = dat,
      ReadError = "Valid data",
      warning = warning)
  }else{
    dat <- tryCatch({
      dat_ <- as.data.frame(mat)
      if(all(dim(dat_)>c(1,1))){
        list(sce = NULL, mae = NULL, dat = as.data.frame(mat), ReadError = "Valid data", warning = "")
      }else{
        list(sce = NULL, mae = NULL, dat = NULL, ReadError = paste("Wrong", typ , "type", sep = " "), warning = "")
      }
    }, error = function(e) {
      list(sce = NULL, mae = NULL, dat = NULL, ReadError = paste("Wrong", typ , "type", sep = " "), warning = "")
    })
  }

  return(dat)

}

# Read rds and consider it a csv
#'
#' @noRd
read_input.rds <- function(filename, ...){

  mat <- tryCatch({
    readRDS(filename)
  }, error = function(e) {
    NULL
  })

  return(read_input.object(mat, typ = "file"))
}

# Function turning a matrix type object to SingleCellExperiment class
#'
#' @noRd
read_input.csv <- function(filename, ...){

  # Load file as matrix using readr and tibble
  dat <- tryCatch({
    list(sce = NULL, mae = NULL, dat = as.data.frame(utils::read.csv(file = filename, header = T)), ReadError = "Valid data", warning = "")
  }, error = function(e) {
    list(sce = NULL, mae = NULL, dat = NULL, ReadError = "Wrong file type", warning = "")
  })

  return(dat)
}

# Check dimensions
#'
#' @noRd
check_dim <- function(x, collimit){
  if(is.null(x$dat)){
    return(x)
  }

  if(nrow(x$dat)==0){
    return(list(sce = NULL, mae = NULL, dat = NULL, ReadError = "Empty data", warning = ""))
  }

  if(ncol(x$dat)>collimit){
    return(list(sce = NULL, mae = NULL, dat = NULL, ReadError = "Too many columns", warning = ""))
  }
  return(x)
}
