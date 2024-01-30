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
rose_plot <- function(norm, data, selected, n_petals = 10, title = NULL, valley = NULL, palette="RdYlGn", quant = c(1,6)){

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
    find_markers(extra = 0, n = n, mat = fexp, quant = quant, zero = valley)
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

#' rose_plot_internal
#'
#' @description A fct function to generate a rose plot
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @noRd
rose_plot_internal <- function(data, selected, title = " ", maintitle = NULL, palette="RdYlGn", colortitle = F){

  m = 11
  n = 10

  fontcolor = "#3D405B"
  fontsize = 14

  # browser()
  data <- data %>% dplyr::filter(variable %in% selected)

  # calculate the ANGLE of the labels
  extradist <- 0.2
  number_of_bar <- nrow(data)

  data$id <- seq(1, number_of_bar)
  angle <-  90 - 360 * (data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  data$hjust<-ifelse( angle < -90, 1, 0)

  # flip angle BY to make them readable
  data$angle<-ifelse(angle < -90, angle+180, angle)

  p <- ggplot(data=data,aes(x=factor(id, levels=id, labels=variable),y=y, fill = factor(color, levels = 1:m)))+
    geom_bar(stat="identity", alpha = 0.5)+
    geom_text(aes(label = variable, y=y+extradist, hjust=hjust, angle=angle), size = 4, color = fontcolor) +
    scale_fill_brewer(type = "div", palette = palette, direction = -1) +
    coord_polar(start = 0, clip = "off")+
    xlab("")+ylab("") +
    theme(
      panel.grid = element_line(linewidth = 0.3, colour = "gray80"),
      plot.margin=margin(grid::unit(0, "cm")),
      panel.spacing = element_blank() ,
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = fontsize-2, colour = fontcolor),
      panel.background = element_rect(fill=NA),
      plot.background = element_rect(fill=NA, color=NA), #,panel.border = element_blank()
      plot.title=element_text(hjust=0, vjust=0.5, size = fontsize-1, colour = fontcolor, face = "italic"),
      plot.subtitle = element_text(hjust=1, vjust=0.5, size = fontsize-3, colour = fontcolor, face = "italic"),
      plot.title.position = "plot"
    )

  if(colortitle){
    p <- p +
      ggtitle(label = maintitle) +
      theme(plot.title = element_markdown(),
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
