#' plotsize
#'
#' @description A utils function to size plots with limits
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plotsize <- function(number){
  height <- number*30
  if(height<600){
    return(600)
  }else{
    return(height)
  }
}

#' labelMandatory
#'
#' @description A utils function to add little color star to the label
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#' firstUp
#'
#' @description A utils function to capitalize the first letter of a word while keeping acronyms
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
firstUp <- function(x) {
   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
   x
}

#' sentenceUp
#'
#' @description A utils function to capitalize the first letter of a word while keeping acronyms
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
sentenceUp <- function(x) {
  x <- paste(firstUp(strsplit(x, split = " ")[[1]]), collapse = " ")
  x
}

#' rsym
#'
#' @description A NULL resistent sym function
#'
#' @return Same as sym from rlang, just allowing NULL values and returning strings intead
#'
#' @noRd
#'
rsym <- function(x) {
  if(is.null(x)){
    return("NULL")
  }else{
    return(paste0("'", as.character(dplyr::sym(x)), "'"))
  }
}

#' run_basic_checks
#'
#' @description A function to run a few checks on the shape of the data
#'
#' @return a NULL value if there are no problems and an error message otherwise.
#'
#' @noRd
#'
run_basic_checks <- function(norm, dat, maxcol = NULL) {

  if(is.null(norm)){
    return("There is something odd regarding the expression data inputed. Please refer to the app's manual and README page for specifications on the input format.")
  }

  if(is.null(rownames(norm))){
    return("Column names are missing in the expression data matrix. Please refer to the app's manual and README page for specifications on the input format.")
  }

  if(is.null(rownames(dat))){
    return("Row names are missing in the data object. Please refer to the app's manual and README page for specifications on the input format.")
  }

  if(!all(rownames(dat) == rownames(norm))){
    return("There is something odd regarding the expression data inputed. Please refer to the app's manual and README page for specifications on the input format.")
  }

  if(is.null(maxcol)){
    return(NULL)
  }

  if(ncol(norm)>maxcol){
    return(paste0("The expression data is entered as ADT data, but the corresponding matrix, with more than ", as.character(maxcol)," columns, looks more like RNA data. Please specify the data type correctly."))
  }

  return(NULL)
}
