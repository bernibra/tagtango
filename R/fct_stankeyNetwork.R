#' load_data
#'
#' @description A fct function to prepare the data for the stankey network
#'
#' @return Returns a list with the different elements for the stankey network.
#'
#' @noRd
load_data <- function(dat, left, right, rna_umap, adt_umap,
                      grouping_variable = NULL, grouping_values = NULL, min_counts = NULL){

  if(!is.null(grouping_values)){
    grouping_values <- if(grouping_values=="All") NULL else {grouping_values}
  }

  if(!is.null(grouping_variable) && !is.null(grouping_values)){

    if(!is.null(rna_umap)){
      rna_umap <- rna_umap[dat[,grouping_variable] == grouping_values,]
    }

    if(!is.null(adt_umap)){
      adt_umap <- adt_umap[dat[,grouping_variable] == grouping_values,]
    }

    dat <- dat %>% dplyr::filter(!!rlang::sym(grouping_variable) == !!grouping_values)

  }

  cells_tagtango <- rownames(dat)
  dat <- dat %>%
    dplyr::group_by(!!sym(left), !!sym(right)) %>%
    dplyr::mutate(n_min_counts = dplyr::n()) %>%
    dplyr::rowwise() %>%
    as.data.frame()
  rownames(dat) <- cells_tagtango

  if(!is.null(min_counts)){

    if(!is.null(rna_umap)){
      rna_umap <- rna_umap[dat$n_min_counts>=min_counts,]
    }

    if(!is.null(adt_umap)){
      adt_umap <- adt_umap[dat$n_min_counts>=min_counts,]
    }

    dat <- dat %>% dplyr::filter(n_min_counts>=min_counts)
  }

  dat$i <- factor(dat[,left], levels = data.frame(x=dat[,left]) %>% dplyr::count(x) %>% dplyr::arrange(n) %>% dplyr::pull(x))
  dat$j <- factor(dat[,right], levels = data.frame(x=dat[,right]) %>% dplyr::count(x) %>% dplyr::arrange(n) %>% dplyr::pull(x))
  dat$idx <- as.numeric(dat$i)-1
  dat$jdx <- as.numeric(dat$j) + max(dat$idx)

  count_mat <- dplyr::count(dat, idx, jdx, i, j)

  nodes = data.frame("name" =
                       c(count_mat %>% dplyr::select(i, idx) %>% dplyr::distinct() %>% dplyr::arrange(idx) %>% dplyr::pull(i),
                         count_mat %>% dplyr::select(j, jdx) %>% dplyr::distinct() %>% dplyr::arrange(jdx) %>% dplyr::pull(j)),
                     "groups" = c(rep("source", count_mat %>% dplyr::select(i, idx) %>% dplyr::distinct() %>% dplyr::arrange(idx) %>% dplyr::pull(i) %>% length()),
                                  rep("target", count_mat %>% dplyr::select(j, jdx) %>% dplyr::distinct() %>% dplyr::arrange(jdx) %>% dplyr::pull(j) %>% length()))
  )

  links = count_mat %>% dplyr::select(idx, jdx, n)

  colnames(links) <- c("source", "target", "value")

  links <- links %>% dplyr::arrange(dplyr::desc(value))

  return(list(dat = dat, links = links, nodes = nodes, rna_umap = rna_umap, adt_umap = adt_umap))
}

#' smallInput
#'
#' @description A fct function to prepare the data for the stankey network
#'
#' @return Returns a list with the different elements for the stankey network.
#'
#' @noRd
smallInput <- function(tag, class = "primary") {
  tag$children[[2]] <- htmltools::tagAppendAttributes(tag$children[[2]], class = class)
  tag
}
