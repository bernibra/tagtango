#' load_data
#'
#' @description A utility function for reading a processing the data as `tagtango`
#' @param filename path to data file
#' @param data_type experiment used in the MultiAssayExperiment or data contained in logcounts in SingleCellExperiment
#' @param left annotation in the left of the diagram
#' @param right annotation in the right of the diagram
#' @param pc1_axis1 first axis of first dimension reduction space
#' @param pc1_axis2 second axis of first dimension reduction space
#' @param pc2_axis1 first axis of second dimension reduction space
#' @param pc2_axis2 second axis of second dimension reduction space
#'
#' @return The return value, if any, from executing the function.
#' @export
process_data <- function(filename, data_type, left, right, pc1_axis1, pc1_axis2, pc2_axis1, pc2_axis2){
  data <- read_input(filename)
  rna <- data.frame(V1 = data$dat[,pc1_axis1], V2 = data$dat[,pc1_axis2])
  adt <- data.frame(V1 = data$dat[,pc2_axis1], V2 = data$dat[,pc2_axis2])

  sce = if(!is.null(data$mae)){data$mae[[data_type]]}else if(!is.null(data$sce)){data$sce}else{NULL}

  dat <- list(
    dat = data$dat,
    left = left,
    right = right,
    sce = sce,
    rna_umap = rna,
    adt_umap = adt,
    data_type = input$data_type
  )

  if(any(gsub("[[:punct:]]", " ", tolower(dat$data_type)) == c("adt", "antibody capture", "protein data", "antibody derived tags", "scadt"))){

    dat$norm <- t(as.matrix(SingleCellExperiment::logcounts(dat$sce)))

  }else if(dat$data_type == "No expression data"){

    dat$norm <- NULL

  }else{

    dat$norm <- dge_rna_data(dat$sce, dat$left, dat$right, numberOFgenes = 5)

    otherproblem <- ifelse(is.null(dat$norm), TRUE, otherproblem)

  }

  return(
      dat
    )
}
