#' colMeans_modified
#'
#' @description A function to avoid errors when the matrix has one row.
#'
#' @return Returns a vector.
#'
#' @noRd
colMeans_b <- function(mat){
  return(
          tryCatch({
            colMeans(mat)
          }, error = function(e) {
            as.vector(mat)
          })
  )
}

#' find_markers
#'
#' @description A function to find the relevant markers
#'
#' @return The list with a data.frame and a list of markers.
#'
#' @noRd
find_markers <- function(extra = 0, n=4, mat, quant = c(1,6), zero = NULL){
  m = 11

  zero <- ifelse(is.null(zero), mean(quant), zero)

  variable <- colnames(mat)
  if(is.null(variable)){
    variable <- names(mat)
  }

  data <- data.frame(variable = variable, value = colMeans_b(mat)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(color = define_color(value, n=m, quant = quant)) %>%
    dplyr::mutate(importance = abs(value-zero)) %>%
    dplyr::mutate(y = extra + value)
  return(list(data = data,
              selected = data %>% dplyr::arrange(value) %>% tail(n) %>% dplyr::pull(variable),
              all = data %>% dplyr::pull(variable)))
}

#' define_color
#'
#' @description A function to define the colors of the rose petals
#'
#' @return A list of colors.
#'
#' @noRd
define_color <- function(value, n = 11, quant = c(1,6)){
  spectral <- seq(1,n)
  idx <- seq(from = quant[1], to=quant[2], length.out=n)[2:(n-1)]
  spectral_ <- seq(1,n)[2:(n-1)]
  if (value>=quant[2]){
    return(spectral[n])
  }else if(value<=quant[1]){
    return(spectral[1])
  }else{
    return(spectral_[which.min(abs(idx-value))])
  }
}

#' find_markers_diff
#'
#' @description A function to find the markers that are most different between two selections
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
find_markers_diff <- function(extra = 0, n=4, mat_left, mat_right, quant = c(1,6), zero = NULL){
  m = 11
  zero <- ifelse(is.null(zero), mean(quant), zero)

  variable <- colnames(mat_left)
  if(is.null(variable)){
    variable <- names(mat_left)
  }

  data <- data.frame(variable = variable, lvalue = colMeans_b(mat_left), rvalue = colMeans_b(mat_right)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(importance = abs(lvalue-rvalue)) %>%
    dplyr::mutate(color = define_color(zero+(lvalue-rvalue), n=m)) %>%
    dplyr::mutate(y = extra + abs(lvalue-rvalue))
  return(list(data = data, selected = data %>% dplyr::arrange(importance) %>% tail(n) %>% dplyr::pull(variable),
              all = data %>% dplyr::pull(variable)))
}

#' Create data.frame for comparision of cell populations
#'
#' @description A fct function to avoid errors when the matrix has one row.
#'
#' @return Returns a data.frame
#'
#' @noRd
data.frame.b <- function(mat_left, label){
  df_left <- tryCatch({
    data.frame(
      variable = colnames(mat_left),
      y0 = sapply(1:ncol(mat_left), function(x) min(mat_left[,x])),
      y25 = sapply(1:ncol(mat_left), function(x) quantile(mat_left[,x], 0.25)),
      y50 = sapply(1:ncol(mat_left), function(x) median(mat_left[,x])),
      y75 = sapply(1:ncol(mat_left), function(x) quantile(mat_left[,x], 0.75)),
      y100 = sapply(1:ncol(mat_left), function(x) max(mat_left[,x])),
      label = label
    )
  }, error = function(e) {
    data.frame(
      variable = names(mat_left),
      y0 = colMeans_b(mat_left),
      y25 = colMeans_b(mat_left),
      y50 = colMeans_b(mat_left),
      y75 = colMeans_b(mat_left),
      y100 = colMeans_b(mat_left),
      label = label
    )
  })

  return(df_left)
}

#' find_markers_diff_PI
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
find_markers_diff_PI <- function(mat_left, mat_right){
  m = 11

  df_left <- data.frame.b(mat_left, "first")
  df_right <- data.frame.b(mat_right, "second")

  data <- rbind(df_left, df_right)
  return(data)
}
