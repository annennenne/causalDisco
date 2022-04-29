#' Evaluate adjacency matrix estimation
#' 
#' Applies several different metrics to evaluate difference between
#' estimated and true adjacency matrices. Intended to be used to evaluate
#' performance of causal discovery algorithms. 
#' 
#' Two options for input are available: Either \code{est} and \code{true}
#' can be two adjacency matrices, or they can be two arrays of adjacency matrices.
#' The arrays should have shape \eqn{n * p * p} where n is the number of of matrices, 
#' and p is the number of nodes/variables. 
#' 
#' The metrics should be given as a list with slots \code{$adj}, \code{$dir} and
#' \code{$other}. Metrics under \code{$adj} are applied to the adjacency confusion
#' matrix, while metrics under \code{$dir} are applied to the conditional orientation
#' confusion matrix (see \link{confusion}). Metrics under \code{$other} are applied 
#' without computing confusion matrices first. 
#' 
#' Available metrics to be used with confusion matrices are \link{precision}, \link{recall},
#' \link{specificity}, \link{FOR}, \link{FDR}, \link{NPV}, \link{F1} and \link{G1}. The user
#' can supply custom metrics as well: They need to have the confusion matrix as their first 
#' argument and should return a numeric. 
#' 
#' Available metrics to be used as "other" is: \link{shd}. The user
#' can supply custom metrics as well: They need to have arguments \code{est_amat} and \code{true_amat},
#' where the former is the estimated adjacency matrix and the latter is the true adjacency matrix. The
#' metrics should return a numeric. 
#' 
#' @param est Estimated adjacency matrix/matrices.
#' @param true True adjacency matrix/matrices.
#' @param metrics List of metrics, see details. 
#' @param ... Further arguments that depend on input type.  Currently only \code{df.out} is allowed, which provides
#' output as a data.frame for single matrix comparisons. 
#'  
#' @export
evaluate <- function(est, true, metrics, ...) {
  UseMethod("evaluate")
}


#' @inherit evaluate
#' 
#' @param  df.out If \code{TRUE} (default), output is returned as a data.frame, otherwise
#' it will be a list. 
#' 
#' @export
evaluate.matrix <- function(est, true, metrics, df.out = TRUE, ...) {
  #browser()
  adj <- metrics$adj
  dir <- metrics$dir
  other <- metrics$other
  
  n_adj <- length(adj)
  n_dir <- length(dir)
  n_other <- length(other)
  
  adj_metrics <- list(adj)
  dir_metrics <- list(dir)
  other_metrics <- list(other)
  
  adj_names <- dir_names <- other_names <- NULL
  
  if (n_adj > 0) {
    adj_conf <- adj_confusion(est, true)
    for (i in 1:n_adj) {
      adj_metrics[[i]] <- do.call(adj[i], list(confusion = adj_conf))
    }
    adj_names <- paste0("adj_", adj, sep = "")
    
  }
  if (n_dir > 0) {
    dir_conf <- dir_confusion(est, true)
    for (i in 1:n_dir) {
      dir_metrics[[i]] <- do.call(dir[i], list(confusion = dir_conf))
    }
    dir_names <- paste0("dir_", dir, sep = "")
  }
  if (n_other > 0) {
    for (i in 1:n_other) {
      other_metrics[[i]] <- do.call(other[i], list(est_amat = est, true_amat = true))
    }
    other_names <- other
  }
  if (df.out) {
    out <- unlist(c(adj_metrics, dir_metrics, other_metrics))
    names(out) <- c(adj_names, dir_names, other_names)
    return(out)
  } else {
    names(adj_metrics) <- adj
    names(dir_metrics) <- dir
    names(other_metrics) <- other
    return(list(adj = adj_metrics, dir = dir_metrics, other = other_metrics))
  }
}


#' @inherit evaluate
#' @export
evaluate.array <- function(est, true, metrics, ...) {
  n <- dim(est)[1]
  p <- length(metrics$adj) + length(metrics$dir) + length(metrics$other)
  out <- matrix(NA, n, p)
  for (i in 1:n) {
    res <- evaluate.matrix(est = est[i, , ], true = true[i, , ], metrics = metrics)
    out[i, ] <- res
  }
  colnames(out) <- names(res)
  data.frame(out)
}
