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
