#' @title Confusion Matrix
#'
#' @description
#' Compute confusion matrix for two PDAG `caugi` graphs.
#'
#' @param truth A `caugi` object representing the truth graph.
#' @param est A `caugi` object representing the estimated graph.
#' @param type Character string specifying the comparison type:
#'   \itemize{
#'     \item \code{"adj"}: adjacency comparison.
#'     \item \code{"dir"}: orientation comparison conditional on shared adjacencies.
#'   }
#'
#' @details Adjacency comparison: The confusion matrix is a cross-tabulation
#' of adjacencies. Hence, a truth positive means that the two inputs agree on
#' the presence of an adjacency. A truth negative means that the two inputs agree
#' on no adjacency. A false positive means that the estimated graph places an adjacency
#' where there should be none. A false negative means that the estimated graph does
#' not place an adjacency where there should have been one.
#'
#' Orientation comparison: The orientation confusion matrix is conditional on agreement on
#' adjacency. This means that only adjacencies that are shared in both input matrices are
#' considered, and agreement wrt. orientation is then computed only among these edges
#' that occur in both matrices. A truth positive is a correctly placed arrowhead (1),
#' a false positive marks placement of arrowhead (1) where there should have been a tail (0),
#' a false negative marks placement of tail (0) where there should have been an arrowhead (1),
#' and a truth negative marks correct placement of a tail (0).
#'
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
#'
#' @return A list with entries \code{tp} (truth positives), \code{tn} (truth negatives),
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
confusion <- function(truth, est, type = c("adj", "dir")) {
  type <- match.arg(type)
  if (!inherits(est, "caugi::caugi") || !inherits(truth, "caugi::caugi")) {
    stop("Both inputs must be caugi objects.", call. = FALSE)
  }
  caugi::same_nodes(truth, est, throw_error = truth)

  # Extract adjacency matrices
  est_amat <- caugi::as_adjacency(est)
  truth_amat <- caugi::as_adjacency(truth)

  nodes <- union(rownames(est_amat), rownames(truth_amat))

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
  tru <- expand_amat(truth_amat, nodes)

  if (type == "adj") {
    adj_confusion(est, tru)
  } else {
    dir_confusion(est, tru)
  }
}


#' @title Evaluate Causal Graph Estimates
#'
#' @description
#' Computes various metrics to evaluate the difference between estimated
#' and truth causal graph. Designed primarily for assessing the
#' performance of causal discovery algorithms.
#'
#' Metrics are supplied as a list with three slots: \code{$adj}, \code{$dir}, and \code{$other}.
#' \describe{
#'   \item{\code{$adj}}{Metrics applied to the adjacency confusion matrix (see [confusion()]).}
#'   \item{\code{$dir}}{Metrics applied to the conditional orientation confusion matrix (see [confusion()]).}
#'   \item{\code{$other}}{Metrics applied directly to the adjacency matrices without computing confusion matrices.}
#' }
#'
#' Adjacency confusion matrix and conditional orientation confusion matrix only works for
#' `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
#'
#' @param truth truth `caugi` object.
#' @param est Estimated `caugi` object.
#' @param metrics List of metrics, see details. If \code{metrics = "all"}, all available metrics are computed.
#'
#' @return A data.frame with one column for each computed metric. Adjacency metrics are prefixed with "adj_",
#' orientation metrics are prefixed with "dir_", other metrics do not get a prefix.
#'
#' @examples
#' cg1 <- caugi::caugi(A %-->% B + C)
#' cg2 <- caugi::caugi(B %-->% A + C)
#' evaluate(cg1, cg2)
#' evaluate(
#'   cg1,
#'   cg2,
#'   metrics = list(
#'     adj = c("precision", "recall"),
#'     dir = c("f1_score"),
#'     other = c("shd")
#'   )
#' )
#'
#' @family metrics
#' @concept metrics
#' @export
evaluate <- function(truth, est, metrics = "all") {
  caugi::same_nodes(truth, est, throw_error = truth)
  .resolve_cm_metric <- function(m) {
    get(paste0(".", m, "_cm"), mode = "function")
  }

  if (identical(metrics, "all")) {
    metrics <- .metric_registry
  } else {
    metrics <- utils::modifyList(
      list(adj = character(), dir = character(), other = character()),
      metrics
    )

    for (slot in names(metrics)) {
      invalid <- setdiff(metrics[[slot]], .metric_registry[[slot]])
      if (length(invalid) > 0) {
        stop(
          sprintf(
            "Invalid %s metric(s): %s. Must be one of: %s",
            slot,
            paste(invalid, collapse = ", "),
            paste(.metric_registry[[slot]], collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  }

  adj <- metrics$adj %||% character()
  dir <- metrics$dir %||% character()
  other <- metrics$other %||% character()

  adj_metrics <- dir_metrics <- other_metrics <- list()
  adj_names <- dir_names <- other_names <- character()

  if (length(adj) > 0) {
    adj_conf <- confusion(truth, est, type = "adj")
    for (m in adj) {
      adj_metrics[[length(adj_metrics) + 1]] <- .resolve_cm_metric(m)(adj_conf)
    }
    adj_names <- paste0("adj_", adj)
  }

  if (length(dir) > 0) {
    dir_conf <- confusion(truth, est, type = "dir")
    for (m in dir) {
      dir_metrics[[length(dir_metrics) + 1]] <- .resolve_cm_metric(m)(dir_conf)
    }
    dir_names <- paste0("dir_", dir)
  }

  if (length(other) > 0) {
    for (m in other) {
      if (m == "shd") {
        other_metrics[[length(other_metrics) + 1]] <- shd(truth, est)
      } else if (m == "hd") {
        other_metrics[[length(other_metrics) + 1]] <- hd(truth, est)
      } else if (m == "aid") {
        other_metrics[[length(other_metrics) + 1]] <- aid(truth, est)
      }
    }
    other_names <- other
  }

  out <- unlist(c(adj_metrics, dir_metrics, other_metrics), use.names = FALSE)
  names(out) <- c(adj_names, dir_names, other_names)
  as.data.frame(as.list(out))
}


.metric_registry <- list(
  adj = c(
    "precision",
    "recall",
    "specificity",
    "false_omission_rate",
    "fdr",
    "npv",
    "f1_score",
    "g1_score"
  ),
  dir = c(
    "precision",
    "recall",
    "specificity",
    "false_omission_rate",
    "fdr",
    "npv",
    "f1_score",
    "g1_score"
  ),
  other = c("shd", "hd", "aid")
)

#' Precision
#'
#' @description Computes precision from two PDAG `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' precision as `TP/(TP + FP)`, where TP are truth positives and
#' `FP` are false positives. If `TP + FP = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
precision <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .precision_cm(cm)
}

.precision_cm <- function(cm) {
  tp <- cm$tp
  fp <- cm$fp
  if (tp + fp != 0) tp / (tp + fp) else 1
}

#' Recall
#'
#' @description Computes recall from two PDAG `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' recall as `TP/(TP + FN)`, where `TP` are truth positives and
#' `FN` are false negatives. If `TP + FN = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
recall <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .recall_cm(cm)
}

.recall_cm <- function(cm) {
  tp <- cm$tp
  fn <- cm$fn
  if (tp + fn != 0) tp / (tp + fn) else 1
}

#' Specificity
#'
#' @description Computes specificity from two PDAG `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' specificity as `TN/(TN + FP)`, where `TN` are truth negatives and
#' `FP` are false positives. If `TN + FP = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
specificity <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .specificity_cm(cm)
}

.specificity_cm <- function(cm) {
  tn <- cm$tn
  fp <- cm$fp
  ifelse(tn + fp != 0, tn / (tn + fp), 1)
}

#' False Omission Rate
#'
#' @description Computes false omission rate from two PDAG `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' false omission rate as `FN/(FN + TN)`, where `FN` are false negatives and
#' `TN` are truth negatives. If `FN + TN = 0, 1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
false_omission_rate <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .false_omission_rate_cm(cm)
}

.false_omission_rate_cm <- function(cm) {
  fn <- cm$fn
  tn <- cm$tn
  ifelse(fn + tn != 0, fn / (fn + tn), 1)
}

#' False Discovery Rate
#'
#' @description Computes false discovery rate from two PDAG `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' false discovery rate as `FP/(FP + TP)`, where `FP` are false positives and
#' `TP` are truth positives. If `FP + TP = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
fdr <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .fdr_cm(cm)
}

.fdr_cm <- function(cm) {
  fp <- cm$fp
  tp <- cm$tp
  ifelse(fp + tp != 0, fp / (fp + tp), 1)
}

#' Negative Predictive Value
#'
#' @description Computes negative predictive value from two PDAG `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' negative predictive value as `TN/(TN + FN)`, where `TN` are truth negatives and
#' `FN` are false negatives. If `TN + FN = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
npv <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .npv_cm(cm)
}

.npv_cm <- function(cm) {
  tn <- cm$tn
  fn <- cm$fn
  ifelse(tn + fn != 0, tn / (tn + fn), 1)
}

#' F1 score
#'
#' @description Computes F1 score from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' F1 score as \eqn{2 \cdot TP/(2 \cdot TP + FP + FN)}, where `TP` are truth positives,
#' `FP` are false positives, and `FN` are false negatives. If `TP + FP + FN = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
f1_score <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .f1_score_cm(cm)
}

.f1_score_cm <- function(cm) {
  tp <- cm$tp
  fp <- cm$fp
  fn <- cm$fn
  ifelse(tp + fp + fn != 0, 2 * tp / (2 * tp + fp + fn), 1)
}

#' G1 score
#'
#' @description Computes G1 score from two `caugi` objects.
#' It converts the `caugi` objects to adjacency matrices and computes
#' G1 score defined as \eqn{2 \cdot TN/(2 \cdot TN + FN + FP)}, where `TN` are truth negatives,
#' `FP` are false positives, and FN are false negatives. If `TN + FN + FP = 0`, `1` is returned.
#' Only supports `caugi` objects with these edge types present `-->`, `<-->`, `---` and no edge.
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
g1_score <- function(truth, est, type = c("adj", "dir")) {
  caugi::same_nodes(truth, est, throw_error = TRUE)
  type <- match.arg(type)
  cm <- confusion(truth, est, type = type)
  .g1_score_cm(cm)
}

.g1_score_cm <- function(cm) {
  tn <- cm$tn
  fn <- cm$fn
  fp <- cm$fp
  ifelse(tn + fn + fp != 0, 2 * tn / (2 * tn + fn + fp), 1)
}

#' @importFrom caugi shd
#' @family metrics
#' @concept metrics
#' @export
caugi::shd

#' @importFrom caugi hd
#' @concept metrics
#' @export
caugi::hd

#' @importFrom caugi aid
#' @concept metrics
#' @export
caugi::aid
