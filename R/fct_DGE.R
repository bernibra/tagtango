#' DGE
#'
#' @description A fct function that performs DGE analysis on the expression matrix. This function can be improved substantially, and I should do so in the next versions of the software.
#'
#' @return Returns an expression matrix with relevant genes.
#'
#' @noRd
dge_rna_data <- function(sce, dat, left, right, numberOFgenes = 10){

  leftgenes <- tryCatch({
    leftmat <- scran::scoreMarkers(sce, dat[,left])
    lapply(leftmat, function(x){
      ordered <- x[order(x$median.logFC.cohen,decreasing=TRUE),]
      ordered %>% utils::head(numberOFgenes) %>% rownames()
    }) %>% unlist()  }, error = function(e) {
    NULL
  })

  names(leftgenes) <- NULL

  rightgenes <- tryCatch({
    rightmat <- scran::scoreMarkers(sce, dat[,right])
    lapply(rightmat, function(x){
      ordered <- x[order(x$median.logFC.cohen,decreasing=TRUE),]
      ordered %>% utils::head(numberOFgenes) %>% rownames()
    }) %>% unlist()}, error = function(e) {
    NULL
  })

  names(rightgenes) <- NULL

  return(unique(c(leftgenes, rightgenes)))
}
