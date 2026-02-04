#' @title Confusion Matrix
#'
#' @description
#' Compute confusion matrix for two `caugi` graphs.
#'
#' @param truth A `caugi` object representing the true graph.
#' @param guess A `caugi` object representing the estimated graph.
#' @param type Character string specifying the comparison type:
#'   \itemize{
#'     \item \code{"adj"}: adjacency comparison.
#'     \item \code{"dir"}: orientation comparison conditional on shared adjacencies.
#'   }
#'
#' @details Adjacency comparison: The confusion matrix is a cross-tabulation
#' of adjacencies. Hence, a true positive means that the two inputs agree on
#' the presence of an adjacency. A true negative means that the two inputs agree
#' on no adjacency. A false positive means that the estimated graph places an adjacency
#' where there should be none. A false negative means that the estimated graph does
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
#' @return A list with entries \code{tp} (true positives), \code{tn} (true negatives),
#' \code{fp} (false positives), and \code{fn} (false negatives).
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' confusion(cg1, cg2)
#' confusion(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
confusion <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  if (!inherits(guess, "caugi::caugi") || !inherits(truth, "caugi::caugi")) {
    stop("Both inputs must be caugi objects.", call. = FALSE)
  }

  # Extract adjacency matrices
  est_amat <- caugi::as_adjacency(guess)
  true_amat <- caugi::as_adjacency(truth)

  nodes <- union(rownames(est_amat), rownames(true_amat))

  # Helper to expand an adjacency matrix to full node set
  expand_amat <- function(A, nodes) {
    out <- matrix(
      0L,
      nrow = length(nodes),
      ncol = length(nodes),
      dimnames = list(nodes, nodes)
    )
    out[rownames(A), colnames(A)] <- A
    out
  }

  est <- expand_amat(est_amat, nodes)
  tru <- expand_amat(true_amat, nodes)

  if (type == "adj") {
    adj_confusion(est, tru)
  } else {
    dir_confusion(est, tru)
  }
}

# TODO: Make dispatch on caugi objects, so we can dispatch on knowledgeable_caugi.
#' Precision
#'
#' @description Computes precision from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' precision as `TP/(TP + FP)`, where TP are true positives and
#' `FP` are false positives. If `TP + FP = 0`, `1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in `[0,1]`.
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' precision(cg1, cg2, type = "adj")
#' precision(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
precision <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  tp <- cm$tp
  fp <- cm$fp
  ifelse(tp + fp != 0, tp / (tp + fp), 1)
}

#' Recall
#'
#' @description Computes recall from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' recall as `TP/(TP + FN)`, where `TP` are true positives and
#' `FN` are false negatives. If `TP + FN = 0`, `1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in `[0,1]`.
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' recall(cg1, cg2, type = "adj")
#' recall(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
recall <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  tp <- cm$tp
  fn <- cm$fn
  ifelse(tp + fn != 0, tp / (tp + fn), 1)
}

#' Specificity
#'
#' @description Computes specificity from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' specificity as `TN/(TN + FP)`, where `TN` are true negatives and
#' `FP` are false positives. If `TN + FP = 0`, `1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in \[0,1\].
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' specificity(cg1, cg2, type = "adj")
#' specificity(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
specificity <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  tn <- cm$tn
  fp <- cm$fp
  ifelse(tn + fp != 0, tn / (tn + fp), 1)
}

#' False Omission Rate
#'
#' @description Computes false omission rate from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' false omission rate as `FN/(FN + TN)`, where `FN` are false negatives and
#' `TN` are true negatives. If `FN + TN = 0, 1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in \[0,1\].
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' false_omission_rate(cg1, cg2, type = "adj")
#' false_omission_rate(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
false_omission_rate <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  fn <- cm$fn
  tn <- cm$tn
  ifelse(fn + tn != 0, fn / (fn + tn), 1)
}

#' False Discovery Rate
#'
#' @description Computes false discovery rate from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' false discovery rate as `FP/(FP + TP)`, where `FP` are false positives and
#' `TP` are true positives. If `FP + TP = 0`, `1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in \[0,1\].
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' fdr(cg1, cg2, type = "adj")
#' fdr(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
fdr <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  fp <- cm$fp
  tp <- cm$tp
  ifelse(fp + tp != 0, fp / (fp + tp), 1)
}

#' Negative Predictive Value
#'
#' @description Computes negative predictive value from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' negative predictive value as `TN/(TN + FN)`, where `TN` are true negatives and
#' `FN` are false negatives. If `TN + FN = 0`, `1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in \[0,1\].
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' npv(cg1, cg2, type = "adj")
#' npv(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
npv <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  tn <- cm$tn
  fn <- cm$fn
  ifelse(tn + fn != 0, tn / (tn + fn), 1)
}

#' F1 score
#'
#' @description Computes F1 score from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' F1 score as \eqn{2 * TP/(2 * TP + FP + FN)}, where `TP` are true positives,
#' `FP` are false positives, and `FN` are false negatives. If `TP + FP + FN = 0`, `1` is returned.
#'
#' @inheritParams confusion
#'
#' @return A numeric in \[0,1\].
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' f1_score(cg1, cg2, type = "adj")
#' f1_score(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
f1_score <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  tp <- cm$tp
  fp <- cm$fp
  fn <- cm$fn
  ifelse(tp + fp + fn != 0, 2 * tp / (2 * tp + fp + fn), 1)
}

#' G1 score
#'
#' @description Computes G1 score from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' G1 score defined as \eqn{2 * TN/(2 * TN + FN + FP)}, where `TN` are true negatives,
#' `FP` are false positives, and FN are false negatives. If `TN + FN + FP = 0`, `1` is returned.
#'
#' @references Petersen, Anne Helby, et al. "Causal discovery for observational sciences using
#' supervised machine learning." arXiv preprint arXiv:2202.12813 (2022).
#'
#' @inheritParams confusion
#'
#' @return A numeric in \[0,1\].
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' g1_score(cg1, cg2, type = "adj")
#' g1_score(cg1, cg2, type = "dir")
#'
#' @family metrics
#' @concept metrics
#' @export
g1_score <- function(truth, guess, type = c("adj", "dir")) {
  type <- match.arg(type)
  cm <- confusion(truth, guess, type = type)
  tn <- cm$tn
  fn <- cm$fn
  fp <- cm$fp
  ifelse(tn + fn + fp != 0, 2 * tn / (2 * tn + fn + fp), 1)
}

#' @title Structural Hamming Distance
#'
#' @description Computes the Structural Hamming Distance (SHD) between two `caugi` objects.
#' SHD is the number of edge additions, deletions, or orientation flips required to
#' transform one graph into the other. Adjacency errors are counted fully, while
#' orientation errors on shared adjacencies are counted as half an error, following
#' the standard convention for partially directed graphs.
#'
#' @inheritParams confusion
#'
#' @return A numeric representing the SHD.
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' shd(cg1, cg2)
#'
#' @family metrics
#' @concept metrics
#' @export
shd <- function(truth, guess) {
  adj <- confusion(truth, guess, type = "adj")
  dir <- confusion(truth, guess, type = "dir")

  adj$fp + adj$fn + (dir$fp + dir$fn) / 2
}
