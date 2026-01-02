#' Compute confusion matrix for comparing two adjacency matrices
#'
#' @description Two adjacency matrices are compared either in terms of adjacencies
#' (\code{type = "adj"}) or orientations (\code{type = "dir"}).
#'
#' @details Adjacency comparison: The confusion matrix is a cross-tabulation
#' of adjacencies. Hence, a true positive means that the two inputs agree on
#' the presence of an adjacency. A true negative means that the two inputs agree
#' on no adjacency. A false positive means that \code{est_amat} places an adjacency
#' where there should be none. A false negative means that \code{est_amat} does
#' not place an adjacency where there should have been one.
#'
#' Orientation comparison: The orientation confusion matrix is conditional on agreement on
#' adjacency. This means that only adjacencies that are shared in both input matrices are
#' considered, and agreement wrt. orientation is then computed only among these edges
#' that occur in both matrices. A true positive is a correctly placed arrowhead (1),
#' a false positive marks placement of arrowhead (1) where there should have been a tail (0),
#' a false negative marks placement of tail (0) where there should have been an arrowhead (1),
#' and a true negative marks correct placement of a tail (0).
#'
#' @return A list with entries \code{$tp} (number of true positives),  \code{$tn} (number of true negatives),
#' \code{$fp} (number of false positives), and  \code{$tp} (number of false negatives).
#'
#' @param est_amat The estimated adjacency matrix, or \code{tpdag}/\code{cpdag}
#' object as obtained from \link{tpc} or \link{pc}
#' @param true_amat The true adjacency matrix, or \code{tpdag}/\code{cpdag}
#' object as obtained from \link{tpc} or \link{pc}
#' @param type String indicating whether the confusion matrix should be computed for adjacencies
#' (\code{"adj"}, the default) or for (conditional) orientations (\code{dir}).
#'
#' @examples
#' x1 <- matrix(c(
#'   0, 0, 0, 0,
#'   1, 0, 1, 0,
#'   1, 0, 0, 0,
#'   0, 0, 1, 0
#' ), 4, 4, byrow = TRUE)
#' x2 <- matrix(c(
#'   0, 0, 1, 0,
#'   1, 0, 0, 0,
#'   0, 0, 0, 0,
#'   1, 0, 1, 0
#' ), 4, 4, byrow = TRUE)
#' nodes <- c("A", "B", "C", "D")
#' rownames(x1) <- colnames(x1) <- nodes
#' rownames(x2) <- colnames(x2) <- nodes
#'
#' # confusion matrix for adjacencies
#' confusion(x2, x1)
#'
#' # confusion matrix for conditional orientations
#' confusion(x2, x1, type = "dir")
#'
#' @export
confusion <- function(est_amat, true_amat, type = "adj") {
  est <- .as_tamat_any(est_amat)
  tru <- .as_tamat_any(true_amat)
  out <- .align_tamat_nodes(est, tru)
  est <- out$est
  tru <- out$tru
  if (type == "adj") {
    adj_confusion(est, tru)
  } else if (type == "dir") {
    dir_confusion(est, tru)
  } else {
    stop("Type must be either 'adj' or 'dir'.")
  }
}

# helpers

.as_tamat_any <- function(x) {
  if (inherits(x, "tamat")) {
    return(x)
  }

  if (is.matrix(x)) {
    nodes <- rownames(x)
    if (is.null(nodes)) {
      nodes <- colnames(x)
    }
    if (is.null(nodes)) {
      nodes <- paste0("V", seq_len(nrow(x)))
    }
    if (inherits(x, "amat.pag")) {
      return(tamat(x, order = nodes, type = "ag"))
    }
    if (inherits(x, "amat.cpdag")) {
      return(tamat(x, order = nodes, type = "pdag"))
    }
    vals <- sort(unique(as.integer(x)))
    if (all(vals %in% c(0L, 1L))) {
      return(tamat(x, order = nodes, type = "pdag"))
    }
    if (all(vals %in% c(0L, 1L, 2L, 3L))) {
      return(tamat(x, order = nodes, type = "ag"))
    }
    stop("Matrix values not recognized as CPDAG or PAG encoding.")
  }
}

.align_tamat_nodes <- function(est, tru) {
  rn_e <- rownames(est)
  rn_t <- rownames(tru)
  if (!length(rn_e) || !length(rn_t)) {
    stop("Both inputs must have row and column names.")
  }
  if (!setequal(rn_e, rn_t)) {
    stop("Inputs must refer to the same node set.")
  }
  tru <- tru[rn_e, rn_e, drop = FALSE]
  list(est = est, tru = tru)
}


# Changed from generic function to allow for class matching for
# both of the first two arguments (i.e. compare amat with tpdag)
# #'@export
# confusion.default <- function(est_amat, true_amat, type = "adj") {
#  if (type == "adj") {
#    adj_confusion(est_amat, true_amat)
#  } else if (type == "dir") {
#    dir_confusion(est_amat, true_amat)
#  } else {
#    stop("Type must be either adj or dir.")
#  }
# }

#' @inherit confusion
#' @export
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
#' @export
dir_confusion <- function(est_amat, true_amat) {
  est_edges <- edges(est_amat)
  true_edges <- edges(true_amat)

  true_adj <- c(true_edges$undir, true_edges$dir)
  true_adj <- c(true_adj, lapply(true_adj, rev))

  true_dir <- true_edges$dir
  true_revdir <- lapply(true_dir, rev)
  # true_undir <- true_edges$undir
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


#' @inherit confusion
#' @details This is an old version of the function, included for possible
#' backwards compatibility. Edges are scored as follows: A correctly unoriented edge
#' counts as a true negative (TN). An undirected edge that should have been directed
#' counts as a false negative (FN). A directed edge that should have been
#' undirected counts as a false positive (FP). A directed edge oriented in the
#' correct direction counts as a true positive (TP). A directed edge
#' oriented in the incorrect direction counts as both a false positive (FP)
#' and a false negative (FN).
#' @export
dir_confusion_original <- function(est_amat, true_amat) {
  est_edges <- edges(est_amat)
  true_edges <- edges(true_amat)

  true_adj <- c(true_edges$undir, true_edges$dir)
  true_adj <- c(true_adj, lapply(true_adj, rev))

  true_dir <- true_edges$dir
  true_revdir <- lapply(true_dir, rev)
  # true_undir <- true_edges$undir
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
        dir_tn <- dir_tn + 1
      } else if (thisedge %in% c(true_dir, true_revdir)) {
        # should be directed
        dir_fn <- dir_fn + 1
      }
    }
  }

  # count metrics for directed edges
  for (i in seq_along(est_dir)) {
    thisedge <- est_dir[i]

    if (thisedge %in% true_adj) {
      if (thisedge %in% true_undir) {
        # directed, should be undirected
        dir_fp <- dir_fp + 1
      } else if (thisedge %in% true_dir) {
        # directed in correct direction
        dir_tp <- dir_tp + 1
      }

      if (thisedge %in% true_revdir) {
        # directed in incorrect direction
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
