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
