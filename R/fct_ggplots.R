bg = "#3D405B"
color.background = "#3D405B"
color.blue = "#283618"
color.white = "#F4F1DE"
color.yellow = "#F4BA02"
styletext = paste0("background-color: ", bg, ";")
fontcolor = "#F4F1DE"
fontsize = 14

theme_set(
  theme_bw() +
    # dark_theme_bw(base_family = fontype) +
    theme(text = element_text(size = fontsize, color = fontcolor),
          axis.text = element_text(size = fontsize, color = fontcolor),
          panel.grid = element_blank(),
          #panel.grid = element_line(linewidth = 0.2, colour = "white"),
          # panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1),
          legend.background = element_rect(fill=bg),
          panel.background = element_rect(fill=bg),
          panel.border = element_rect(color = fontcolor, linewidth = 1),
          # plot.margin = unit(c(1,1,1,1), "cm"),
          legend.key = element_rect(colour = NA, fill = bg),
          strip.background.x = element_blank(),
          strip.text.x = element_text(color = fontcolor, size = fontsize),
          plot.background = element_rect(fill=bg, color=NA) #,panel.border = element_blank()
    ))

#' UMAP
#'
#' @description A fct function to plot a UMAP
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @noRd

plot_UMAP <- function(data, labels, sampling = 0.2, values = c("a", "b"), title = "UMAP of the RNA data"){
  # browser()
  data$labels <- labels

  data_ <- data[sample(nrow(data), round(nrow(data) * sampling)), ]

  return(data_ %>% dplyr::arrange(labels) %>% ggplot(aes(x=V1, y=V2, color = labels)) +
           geom_point(size = 0.1, alpha = 0.2) +
           scale_colour_manual(values = values) +
           xlab("first axis") +
           ylab("second axis") +
           ggtitle(label = "", subtitle = title) +
           guides(colour = guide_legend(override.aes = list(size=2, alpha = 0.7))) +
           theme_bw() +
           theme(legend.title = element_blank(), plot.title = element_blank(),
                 legend.background = element_blank(),
                 panel.background = element_rect(fill=NA),
                 text = element_text(size = fontsize-1, color = color.background),
                 axis.text = element_text(size = fontsize-1, color = color.background),
                 panel.border = element_rect(color = color.background, linewidth = 1),
                 legend.position='bottom',
                 legend.justification='right',
                 legend.direction='horizontal',
                 legend.text = element_text(size = fontsize-2),
                 legend.key = element_rect(fill = "transparent", colour = "transparent"),
                 plot.background = element_rect(fill=NA, color=NA), #,panel.border = element_blank()
           ))
}

#' rose_plot
#'
#' @description A fct function to generate a rose plot
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @noRd

rose_plot <- function(data, selected, title = " ", maintitle = NULL, palette="RdYlGn", colortitle = F){
  m = 11
  n = 10
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

  # browser()
  # labels=c("first",rep("", length(unique(data$color))-2), "second")
  p <- ggplot(data=data,aes(x=factor(id, level=id, label=variable),y=y, fill = factor(color, levels = 1:m)))+
    geom_bar(stat="identity", alpha = 0.5)+
    geom_text(aes(label = variable, y=y+extradist, hjust=hjust, angle=angle), size = 4, color = color.background) +
    scale_fill_brewer(type = "div", palette = palette, direction = -1) +
    coord_polar(start = 0, clip = "off")+
    xlab("")+ylab("") +
    # ylim(c(0,7))+
    theme(
      panel.grid = element_line(linewidth = 0.3, colour = "gray80"),
      plot.margin=margin(grid::unit(0, "cm")),
      panel.spacing = element_blank() ,
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = fontsize-2, colour = color.background),
      # axis.ticks = element_blank(),
      panel.background = element_rect(fill=NA),
      plot.background = element_rect(fill=NA, color=NA), #,panel.border = element_blank()
      plot.title=element_text(hjust=0, vjust=0.5, size = fontsize-1, colour = color.background, face = "italic"),
      plot.subtitle = element_text(hjust=1, vjust=0.5, size = fontsize-3, colour = color.background, face = "italic"),
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
