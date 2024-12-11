#' Generate a ggplot bar plot with marker differences
#'
#' @description A function to generate a barlot as `tagtango' does.
#' @param norm normalized expression data with cells as rows and columns as markers/genes (i.e. function `process_data()' should provide this under the attribute `data')
#' @param data data.frame with the different annotations, where each row represent a cell. The rownames should match those of `norm' (i.e. function `process_data()' should provide this under the attribute `network').
#' @param first_selection boolean array with first selection cells as TRUE values, for all cells in `data'.
#' @param second_selection boolean array with second selection cells as TRUE values, for all cells in `data'.
#' @param n_bars if an integer value, it defines the number of bars of the plot, selecting those that are most "relevant". If an array with marker names, it uses those.
#' @param valley the value in `norm' corresponding to the valley separating positive and negative peak for CITE-seq data.
#' @param palette color palette, default "BrBG"
#' @param quant position of positive and negative peak for normalized data.
#'
#' @return returns a list containing a ggplot object and the list of markers selected and used.
#' @export
bar_diff <- function(norm, data, first_selection, second_selection, n_bars = 10, valley = NULL, palette="BrBG", quant = NULL){

  if(is.null(title)){
    title <- ""
  }

  if(is.null(quant)){
    quant <- stats::quantile(as.numeric(norm), probs = c(0.05, 0.95))
  }

  markers <- NULL
  if(methods::is(n_bars, "numeric")){
    n <- n_bars
  }else if(methods::is(n_bars, "character")){
    if(!all(n_bars %in% colnames(norm))){
      stop("Some markers in `n_bars` are not present in `norm'")
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

  data_diff <- tryCatch({
    find_markers_diff(extra = 0, n = n, mat_left = fexp, mat_right = sexp, zero = valley, quant = quant)
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

#' box_diff_internal
#'
#' @description A fct function to generate a box plot
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @noRd
#'
box_diff_internal <- function(data, selected, title = " ", values = c("first" = "red", "second"= "blue"), maintitle = NULL, palette="RdYlGn", colortitle = F){

  variable <- y0 <- y25 <- y50 <- y75 <- y100 <- label <- NULL

  fontcolor = "#3D405B"
  fontsize = 14

  data_ <- data %>% dplyr::filter(variable %in% selected)

  data_$id <- 1:nrow(data_)

  p <- ggplot(data = data_, aes(x= variable, ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100, g = label, fill = label))+
    geom_boxplot(stat = "identity", alpha = 0.4, position=position_dodge(0.6), width=0.5) +
    scale_fill_manual(values = values) +
    theme_bw() +
    theme(      panel.grid = element_blank(),
                axis.text.x = element_text(size = fontsize-2, colour = fontcolor, angle = 45, hjust = 1),
                axis.text.y = element_text(size = fontsize-2, colour = fontcolor),
                axis.title.x = element_blank(),
                panel.background = element_rect(fill=NA),
                plot.background = element_rect(fill=NA, color=NA), #,panel.border = element_blank()
                strip.background = element_blank(),
                strip.text = element_text(size = fontsize-3)
    )

  if(colortitle){
    p <- p +
      ggtitle(label = maintitle) +
      theme(plot.title = ggtext::element_markdown(),
            legend.position = "none")
    return(p)
  }else{
    p <- p + theme(legend.position = "none")
  }

  if(!is.null(maintitle)){
    return(p + ggtitle(label = maintitle, subtitle = title))
  }else{
    return(p + ggtitle(label = "", subtitle = title))
  }

}
