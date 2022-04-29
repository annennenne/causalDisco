#' Compute confusion matrix for comparing two adjacency matrices
#' 
#' Two adjacency matrices are compared either in terms of adjacencies 
#' (\code{type = "adj"}) or orientations (\code{type = "dir"}). 
#' 
#' In the former case, the confusion matrix is a cross-tabulation
#' of adjacencies. 
#' 
#' In the latter case, the orientation confusion matrix is conditional on agreement on 
#' adjacency. This means that only adjacencies that are shared in both input matrices are 
#' considered, and agreement wrt. orientation is then computed only among these edges
#' that occur in both matrices. A true positive is a correctly placed arrowhead (1), 
#' a false positive marks placement of arrowhead (1) where there should have been a tail (0), 
#' a false negative marks placement of tail (0) where there should have been an arrowhead (1),
#' and a true negative marks correct placement of a tail (0). 
#' 
#' @return List with entries \code{$tp} (number of true positives),  \code{$tn} (number of true negatives),
#'\code{$fp} (number of false positives), and  \code{$tp} (number of false negatives). 
#' 
#' @param est_amat The estimated adjacency matrix
#' @param true_amat The true adjacency matrix
#' @param type String indicating whether the confusion matrix should be computed for adjacencies
#' (\code{"adj"}, the default) or for (conditional) orientations (\code{dir}). 
#' 
#' @export
confusion <- function(est_amat, true_amat, type = "adj") {
  if (type == "adj") {
    adj_confusion(est_amat, true_amat)
  } else if (type == "dir") {
    dir_confusion(est_amat, true_amat)
  } else {
    stop("Type must be either adj or dir.")
  }
}


#' @inherit confusion
#' @export
adj_confusion <- function(est_amat, true_amat) {
  # browser()
  est_halfskel <- halfskel(est_amat)
  true_halfskel <- halfskel(true_amat)
  
  list(tp = adj_tp(est_halfskel, true_halfskel), 
       tn = adj_tn(est_halfskel, true_halfskel), 
       fp = adj_fp(est_halfskel, true_halfskel),  
       fn = adj_fn(est_halfskel, true_halfskel))
  
}

#' @inherit confusion
#' @export
dir_confusion <- function(est_amat, true_amat) {
  est_edges <- edges(est_amat)
  true_edges <- edges(true_amat)
  
  true_adj <- c(true_edges$undir, true_edges$dir)
  true_adj <- c(true_adj, lapply(true_adj, rev))
  
  true_dir <- true_edges$dir
  true_revdir <- lapply(true_dir, rev)
  true_undir <- true_edges$undir
  
  est_dir <- est_edges$dir
  est_undir <- est_edges$undir
  
  dir_fp <- 0
  dir_fn <- 0
  dir_tp <- 0
  dir_tn <- 0
  
  #count metrics for undirected edges
  if (length(est_undir) > 0) {
    for (i in 1:length(est_undir)) {
      thisedge <- est_undir[i]
      if (thisedge %in% true_adj) {
        if (thisedge %in% true_undir) { #is correctly undirected
          dir_tn <- dir_tn + 1
        } else if (thisedge %in% c(true_dir, true_revdir)) { #is undirected, should be directed
          dir_fn <- dir_fn + 1
        }
      }
    }
  }
  
  #count metrics for directed edges
  if (length(est_dir) > 0) {
    for (i in 1:length(est_dir)) {
      thisedge <- est_dir[i]
      if (thisedge %in% true_adj) {
        if (thisedge %in% true_undir) { #is directed, should be undirected
          dir_fp <- dir_fp + 1 
        } else if (thisedge %in% true_dir) { #is directed in correct direction
          dir_tp <- dir_tp + 1
        } 
        if (thisedge %in% true_revdir) { #is directed in incorrect direction
          dir_fp <- dir_fp + 1
          dir_fn <- dir_fn + 1
        }
      }
    }
  }
  list(tp = dir_tp, tn = dir_tn,
       fp = dir_fp, fn = dir_fn)
}






###############################################################################################################
# Not exported below###########################################################################################
###############################################################################################################



adj_fp <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 1 & true_halfskel == 0)
}

adj_fn <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 0 & true_halfskel == 1)
}

adj_tp <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 1 & true_halfskel == 1)
}

adj_tn <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 0 & true_halfskel == 0)
}
