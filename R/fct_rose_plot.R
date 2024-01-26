#' Generate a ggplot rose plot with marker expression
#'
#' @description A function to generate a rose plot as `tagtango` does.
#' @param norm normalized expression data with cells as rows and columns as markers/genes (i.e. function `process_data()` should provide this under the attribute `data`)
#' @param data data.frame with the different annotations, where each row represent a cell. The rownames should match those of `norm` (i.e. function `process_data()` should provide this under the attribute `network`).
#' @param selected boolean array with selected cells as True values, for all cells in `data`.
#' @param n_petals if an integer value, it defines the number of petals of the rose plot, selecting those that are most "relevant". If an array with marker names, it uses those.
#' @param title main plot title. Default `NULL`.
#' @param valley the value in `norm` corresponding to the valley separating positive and negative peak for CITE-seq data.
#' @param palette color palette, default "RdYlGn"
#'
#' @return returns a ggplot object.
#' @export
rose_plot <- function(norm, data, selected, title, n_petals = 10, valley = NULL, palette="RdYlGn"){

  if(is.null(title)){
    title <- ""
  }

  markers <- NULL
  if(class(n_petals) == "numeric"){
    n <- n_petals
  }else if(class(n_petals) == "character"){
    if(!all(n_petals %in% colnames(norm))){
      stop("Some markers in `n_petals` are not present in `norm`")
    }
    n <- 10
    markers <- n_petals
  }else{
    stop("Unexpected input for `n_petals`")
  }

  fexp <- norm[(rownames(norm) %in% rownames(data)[selected]), ]
  ncell <- nrow(fexp)

  if(is.null(valley)){
    valley <- mean(colMeans(fexp))
  }

  newdata <- tryCatch({
    find_markers(extra = 0, n = n, mat = fexp, zero = valley)
      }, error = function(e) {
    NULL
    })

  if(is.null(newdata)){
    stop("There was a issue with the input data, please refer to the software manual")
  }

  if(is.null(markers)){
    markers <- newdata$selected
  }

  p <- rose_plot_internal(data = newdata$data, selected = markers,
                          title = ifelse(is.null(ncell), "1 cell", paste0(ncell, " cells")),
                          maintitle = title, palette=palette)

  return(p)
}
