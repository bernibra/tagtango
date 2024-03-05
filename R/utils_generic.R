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

#' find_name_projection
#'
#' @description A function to find sensible names for projections
#'
#' @return A good pair of names.
#'
#' @noRd
find_name_projection <- function(n=1, maxlength=80, maxtab=30, P1a1 = NULL, P1a2 = NULL, P2a1=NULL, P2a2 = NULL){

  projection_number <- paste0("#", as.character(n))
  if(!is.null(P1a1)){
    P1a1_ <- unlist(strsplit(stringr::str_trim(gsub(pattern = "[^[:alnum:]]", " ", P1a1)), " "))
    P1a1 <- tolower(P1a1_)
  }
  if(!is.null(P1a2)){
    P1a2_ <- unlist(strsplit(stringr::str_trim(gsub(pattern = "[^[:alnum:]]", " ", P1a2)), " "))
    P1a2 <- tolower(P1a2_)
  }
  if(!is.null(P2a1)){
    P2a1_ <- unlist(strsplit(stringr::str_trim(gsub(pattern = "[^[:alnum:]]", " ", P2a1)), " "))
    P2a1 <- tolower(P2a1_)
  }
  if(!is.null(P2a2)){
    P2a2_ <- unlist(strsplit(stringr::str_trim(gsub(pattern = "[^[:alnum:]]", " ", P2a2)), " "))
    P2a2 <- tolower(P2a2_)
  }

  if(is.null(P1a1) || is.null(P1a2)){
    return(NULL)
  }

  P1common <- intersect(P1a1, P1a2)
  proj <- paste("Projection", projection_number, sep = " ")

  if(is.null(P2a1) || is.null(P2a2)){
    if(length(P1common)==0){
      tab <- proj
      title <- paste("Projection: ", paste(P1a1, collapse = " "), " VS ", paste(P1a2, collapse = " "), sep = "")
      title <- ifelse(nchar(title)>=maxlength, proj, title)
      return(list(tab = tab, title = title, x=paste(P1a1_, collapse = " "), y = paste(P1a2_, collapse = " ")))
    }else{
      tab <- paste(P1common,collapse = " ")
      tab <- ifelse(nchar(tab)>=maxtab, proj, tab)
      title <- paste("Projection:", paste(P1common,  collapse = " "))
      title <- ifelse(nchar(title)>=maxlength, proj, title)
      return(list(tab = tab, title = title, x=paste(P1a1_, collapse = " "), y = paste(P1a2_, collapse = " ")))
    }
  }

  P2common <- intersect(P2a1, P2a2)
  diff1 <- setdiff(P1common, P2common)

  if(length(diff1)==0){
    tab <- proj
    title <- paste("Projection: ", paste(P1a1, collapse = " "), " VS ", paste(P1a2, collapse = " "), sep = "")
    title <- ifelse(nchar(title)>=maxlength, proj, title)
    return(list(tab = tab, title = title, x=paste(P1a1_, collapse = " "), y = paste(P1a2_, collapse = " ")))
  }else{
    tab <- paste(diff1,collapse = " ")
    tab <- ifelse(nchar(tab)>=maxtab, proj, tab)
    title <- paste("Projection:", paste(diff1,  collapse = " "))
    title <- ifelse(nchar(title)>=maxlength, proj, title)
    return(list(tab = tab, title = title, x=paste(P1a1_, collapse = " "), y = paste(P1a2_, collapse = " ")))
  }

}
