#' DGE
#'
#' @description A fct function that performs DGE analysis on the expression matrix. This function can be improved substantially, and I should do so in the next versions of the software.
#'
#' @return Returns an expression matrix with relevant genes.
#'
#' @noRd
dge_rna_data <- function(sce, left, right, numberOFgenes = 5){
  leftmat <- scran::scoreMarkers(sce, SingleCellExperiment::colData(sce)[,left])
  leftgenes <- lapply(leftmat, function(x){
    ordered <- x[order(x$median.logFC.cohen,decreasing=TRUE),]
    ordered %>% head(numberOFgenes) %>% rownames()
  }) %>% unlist()
  names(leftgenes) <- NULL

  rightmat <- scran::scoreMarkers(sce, SingleCellExperiment::colData(sce)[,right])
  rightgenes <- lapply(rightmat, function(x){
    ordered <- x[order(x$median.logFC.cohen,decreasing=TRUE),]
    ordered %>% head(numberOFgenes) %>% rownames()
  }) %>% unlist()
  names(rightgenes) <- NULL

  genes <- unique(c(leftgenes, rightgenes))
  return(t(as.matrix(SingleCellExperiment::logcounts(sce)[rownames(sce) %in% genes,])))
}
