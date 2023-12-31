#' rose_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
find_markers <- function(extra = 0, n=4, mat, zero = 3){
  m = 11
  # browser()
  data <- data.frame(variable = colnames(mat), value = colMeans(mat)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(color = define_color(value, n=m)) %>%
    dplyr::mutate(importance = abs(value-zero)) %>%
    dplyr::mutate(y = extra + value)
  return(list(data = data,
              selected = data %>% dplyr::arrange(value) %>% tail(n) %>% dplyr::pull(variable),
              all = data %>% dplyr::pull(variable)))
}

#' define_color
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
define_color <- function(value, n = 11){
  spectral <- seq(1,n)
  idx <- seq(from = 1, to=6, length.out=n)[2:(n-1)]
  spectral_ <- seq(1,n)[2:(n-1)]
  if (value>=6){
    return(spectral[1])
  }else if(value<=1){
    return(spectral[n])
  }else{
    return(spectral_[which.min(abs(idx-value))])
  }
}

#' find_markers_diff
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
find_markers_diff <- function(extra = 0, n=4, mat_left, mat_right, zero = 3){
  m = 11
  data <- data.frame(variable = colnames(mat_left), lvalue = colMeans(mat_left), rvalue = colMeans(mat_right)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(importance = abs(lvalue-rvalue)) %>%
    dplyr::mutate(color = define_color(zero+(lvalue-rvalue), n=m)) %>%
    dplyr::mutate(y = extra + abs(lvalue-rvalue))
  return(list(data = data, selected = data %>% dplyr::arrange(importance) %>% tail(n) %>% dplyr::pull(variable),
              all = data %>% dplyr::pull(variable)))
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
  df_left <- data.frame(
    variable = colnames(mat_left),
    y0 = sapply(1:ncol(mat_left), function(x) min(mat_left[,x])),
    y25 = sapply(1:ncol(mat_left), function(x) quantile(mat_left[,x], 0.25)),
    y50 = sapply(1:ncol(mat_left), function(x) median(mat_left[,x])),
    y75 = sapply(1:ncol(mat_left), function(x) quantile(mat_left[,x], 0.75)),
    y100 = sapply(1:ncol(mat_left), function(x) max(mat_left[,x])),
    label = "first"
  )

  df_right <- data.frame(
    variable = colnames(mat_right),
    y0 = sapply(1:ncol(mat_right), function(x) min(mat_right[,x])),
    y25 = sapply(1:ncol(mat_right), function(x) quantile(mat_right[,x], 0.25)),
    y50 = sapply(1:ncol(mat_right), function(x) median(mat_right[,x])),
    y75 = sapply(1:ncol(mat_right), function(x) quantile(mat_right[,x], 0.75)),
    y100 = sapply(1:ncol(mat_right), function(x) max(mat_right[,x])),
    label = "second"
  )

  data <- rbind(df_left, df_right)
  return(data)
}
