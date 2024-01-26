#' Generate a ggplot bar plot with marker differences
#'
#' @description A function to generate a barlot as `tagtango` does.
#' @param norm normalized expression data with cells as rows and columns as markers/genes (i.e. function `process_data()` should provide this under the attribute `data`)
#' @param data data.frame with the different annotations, where each row represent a cell. The rownames should match those of `norm` (i.e. function `process_data()` should provide this under the attribute `network`).
#' @param first_selection boolean array with first selection cells as True values, for all cells in `data`.
#' @param second_selection boolean array with second selection cells as True values, for all cells in `data`.
#' @param n_bars if an integer value, it defines the number of bars of the plot, selecting those that are most "relevant". If an array with marker names, it uses those.
#' @param valley the value in `norm` corresponding to the valley separating positive and negative peak for CITE-seq data.
#' @param palette color palette, default "BrBG"
#'
#' @return returns a list containing a ggplot object and the list of markers selected and used.
#' @export
bar_diff <- function(norm, data, first_selection, second_selection, n_bars = 10, valley = NULL, palette="BrBG"){

  if(is.null(title)){
    title <- ""
  }

  markers <- NULL
  if(class(n_bars) == "numeric"){
    n <- n_bars
  }else if(class(n_bars) == "character"){
    if(!all(n_bars %in% colnames(norm))){
      stop("Some markers in `n_bars` are not present in `norm`")
    }
    n <- 10
    markers <- n_bars
  }else{
    stop("Unexpected input for `n_bars`")
  }

  fexp <- norm[(rownames(norm) %in% rownames(data)[first_selection]), ]
  fncell <- nrow(fexp)

  sexp <- norm[(rownames(norm) %in% rownames(data)[second_selection]), ]
  sncell <- nrow(sexp)

  if(is.null(valley)){
    valley <- mean(colMeans(fexp))
  }

  data_diff <- tryCatch({
    find_markers_diff(extra = 0, n = n, mat_left = fexp, mat_right = sexp, zero = valley)
  }, error = function(e) {
    NULL
  })

  data_diff_ <- tryCatch({
    find_markers_diff_PI(mat_left = fexp, mat_right = sexp)
  }, error = function(e) {
    NULL
  })

  if(is.null(data_diff) || is.null(data_diff_)){
    stop("There was a issue with the input data, please refer to the software manual")
  }

  if(is.null(markers)){
    markers <- data_diff$selected
  }

  p <- box_diff_internal(data = data_diff_,
           selected = markers,
           values = c("first"="#35978f", "second"="#bf812d"),
           title = NULL, maintitle = "Differences: <span style = 'color:#35978f;'>**first**</span> vs <span style = 'color:#bf812d;'>**second**</span> selection",
           palette=palette, colortitle = TRUE)
  return(list(p = p, markers = markers))
}
