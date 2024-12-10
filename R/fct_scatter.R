#' Generate a ggplot scatter plot
#'
#' @description A function to generate a scatter plot as `tagtango' does.
#' @param data data.frame with dimension reduction axes.
#' @param labels array with labels for first (and potentially second selections), for all cells in `data'.
#' @param values boolean array with second selection cells as TRUE values, for all cells in `data'.
#' @param title boolean array with second selection cells as TRUE values, for all cells in `data'.
#' @param xlabel the text for the x axis.
#' @param ylabel the text for the y axis.
#'
#' @return returns a ggplot object.
#' @export
scatter_plot <- function(data, labels, values = c("a", "b"), title = "UMAP of the RNA data", xlabel = "first axis", ylabel = "second axis"){

  V1 <- V2 <- NULL

  fontcolor = "#3D405B"
  fontsize = 14

  data$labels <- labels
  sampling <- 40000

  if( nrow(data) >= sampling ){
    data <- data[sample(nrow(data), sampling), ]
  }

  alpha <- 0.2 + (abs(nrow(data)-sampling)/sampling)*0.6

  return(data %>% dplyr::arrange(labels) %>% ggplot(aes(x=V1, y=V2, color = labels)) +
           geom_point(size = 0.5, alpha = alpha) +
           scale_colour_manual(values = values) +
           xlab(xlabel) +
           ylab(ylabel) +
           ggtitle(label = "", subtitle = title) +
           guides(colour = guide_legend(override.aes = list(size=2, alpha = 0.7))) +
           theme_bw() +
           theme(legend.title = element_blank(), plot.title = element_blank(),
                 legend.background = element_blank(),
                 panel.background = element_rect(fill=NA),
                 text = element_text(size = fontsize-1, color = fontcolor),
                 axis.text = element_text(size = fontsize-1, color = fontcolor),
                 panel.border = element_rect(color = fontcolor, linewidth = 1),
                 legend.position='bottom',
                 legend.justification='right',
                 legend.direction='horizontal',
                 legend.text = element_text(size = fontsize-2),
                 legend.key = element_rect(fill = "transparent", colour = "transparent"),
                 plot.background = element_rect(fill=NA, color=NA), #,panel.border = element_blank()
           ))
}
