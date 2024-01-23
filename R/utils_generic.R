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

