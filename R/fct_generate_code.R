#' Generate code for rose plots
#'
#' @description A function to avoid cluttering the main files too much with text
#'
#' @return Returns a string
#'
#' @noRd
generate_code_rose <- function(norm, fselection, sselection, data_type){
  if(!is.null(sselection) && !is.null(norm)){
    code_figures <- c("## Bar plot\n",
                             paste0("g <- bar_diff(norm = dat$data$norm",
                                    ", data = dat$network$dat",
                                    ", first_selection = first_selection",
                                    ", second_selection = second_selection",
                                    ", n_bars = 10",
                                    ifelse(data_type=="RNA", "", ", valley = 3"),
                                    ")\nprint(g$p)\n"
                             ),
                             "## First rose plot\n",
                             paste0("p1 <- rose_plot(norm = dat$data$norm",
                                    ", data = dat$network$dat",
                                    ", selected = first_selection",
                                    ", n_petals = g$markers",
                                    ifelse(data_type=="RNA", "", ", valley = 3"),
                                    ", title = ftitle",
                                    ")\nprint(p1)\n"
                             ),
                             "## Second rose plot\n",
                             paste0("p2 <- rose_plot(norm = dat$data$norm",
                                    ", data = dat$network$dat",
                                    ", selected = second_selection",
                                    ", n_petals = g$markers",
                                    ifelse(data_type=="RNA", "", ", valley = 3"),
                                    ", title = stitle",
                                    ")\nprint(p2)\n")
    )
  }else if(!is.null(fselection) && !is.null(norm)){
    code_figures <- c("## First rose plot\n",
                             paste0("p1 <- rose_plot(norm = dat$data$norm",
                                    ", data = dat$network$dat",
                                    ", selected = first_selection",
                                    ", n_petals = 10",
                                    ifelse(data_type=="RNA", "", ", valley = 3"),
                                    ", title = ftitle",
                                    ")\nprint(p1)\n"
                             ))
  }else{
    code_figures <- NULL
  }
  return(code_figures)
}

#' Generate code for rose plots
#'
#' @description A function to avoid cluttering the main files too much with text
#'
#' @return Returns a string
#'
#' @noRd
generate_code_umap <- function(df, fselection, sselection, label = "first dimension"){

if(!is.null(sselection)){
    code_figures <- c(paste0("## Scatter plot ", label, "\n"),
                      'labels = c("all other", "first", "second", "both")[1*first_selection + second_selection*2 +1]',
                      'values = c("all other"="#D3D1C6", "first"="#35978f", "second"="#bf812d", "both" = "#808B5A")\n',
                      paste0(substring(label, 1, 1) ,' <- scatter_plot(data = ', df , ', labels = labels, values = values, title = ', rsym(label) , ")\nprint(", substring(label, 1, 1),")\n"))
  }else if(!is.null(fselection)){
    code_figures <- c(paste0("## Scatter plot ", label, "\n"),
                      'labels = ifelse(first_selection, "selected", "other")',
                      'values = c("other"="#D3D1C6", "selected"="#35978f")\n',
                      paste0(substring(label, 1, 1) ,' <- scatter_plot(data = ', df , ', labels = labels, values = values, title = ', rsym(label) , ")\nprint(", substring(label, 1, 1),")\n"))

  }else{
    code_figures <- NULL
  }
  return(code_figures)
}
