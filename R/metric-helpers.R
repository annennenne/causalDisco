#' @title Confusion Matrix Components
#'
#' @description
#' Compute confusion matrix components for adjacency and direction
#' matrices.
#' @param amat An adjacency matrix.
#' @return A numeric vector representing the lower triangle of the
#' adjacency matrix symmetrized (i.e., undirected edges).
#' @keywords internal
#' @noRd
halfskel <- function(amat) {
  out <- amat + t(amat) != 0
  as.numeric(out[lower.tri(out)])
}

#' @inherit confusion
#' @keywords internal
#' @noRd
adj_confusion <- function(est_amat, true_amat) {
  est_halfskel <- halfskel(est_amat)
  true_halfskel <- halfskel(true_amat)

  list(
    tp = adj_tp(est_halfskel, true_halfskel),
    tn = adj_tn(est_halfskel, true_halfskel),
    fp = adj_fp(est_halfskel, true_halfskel),
    fn = adj_fn(est_halfskel, true_halfskel)
  )
}

#' @inherit confusion
#' @keywords internal
#' @noRd
dir_confusion <- function(est_amat, true_amat) {
  est_edges <- edges_amat(est_amat)
  true_edges <- edges_amat(true_amat)

  true_adj <- c(true_edges$undir, true_edges$dir)
  true_adj <- c(true_adj, lapply(true_adj, rev))

  true_dir <- true_edges$dir
  true_revdir <- lapply(true_dir, rev)
  true_undir <- c(true_edges$undir, lapply(true_edges$undir, rev))

  est_dir <- est_edges$dir
  est_undir <- est_edges$undir

  dir_fp <- 0
  dir_fn <- 0
  dir_tp <- 0
  dir_tn <- 0

  # count metrics for undirected edges
  for (i in seq_along(est_undir)) {
    thisedge <- est_undir[i]

    if (thisedge %in% true_adj) {
      if (thisedge %in% true_undir) {
        # correctly undirected
        dir_tn <- dir_tn + 2
      } else if (thisedge %in% c(true_dir, true_revdir)) {
        # undirected, should be directed
        dir_fn <- dir_fn + 1
        dir_tn <- dir_tn + 1
      }
    }
  }

  # count metrics for directed edges
  for (i in seq_along(est_dir)) {
    thisedge <- est_dir[i]

    if (thisedge %in% true_adj) {
      if (thisedge %in% true_undir) {
        # is directed, should be undirected
        dir_fp <- dir_fp + 1
        dir_tn <- dir_tn + 1
      } else if (thisedge %in% true_dir) {
        # is directed in correct direction
        dir_tp <- dir_tp + 1
        dir_tn <- dir_tn + 1
      }

      if (thisedge %in% true_revdir) {
        # is directed in incorrect direction
        dir_fp <- dir_fp + 1
        dir_fn <- dir_fn + 1
      }
    }
  }

  list(
    tp = dir_tp,
    tn = dir_tn,
    fp = dir_fp,
    fn = dir_fn
  )
}

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
