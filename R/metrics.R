#' Precision
#' 
#' Computes precision (aka positive predictive value) from a confusion matrix, 
#' see \link{confusion}. 
#' Precision is defined as TP/(TP + FP), where TP are true positives and 
#' FP are false positives. If TP + FP = 0, 0 is returned. 
#' 
#' @param confusion Confusion matrix as obtained from \link{confusion}
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
precision <- function(confusion) {
  tp <- confusion$tp
  fp <- confusion$fp
  ifelse(tp + fp != 0, tp/(tp + fp), 1)
}

#' Recall
#' 
#' Computes recall from a confusion matrix, see \link{confusion}. 
#' Recall is defined as TP/(TP + FN), where TP are true positives and 
#' FN are false negatives. If TP + FN = 0, 0 is returned. 
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
recall <- function(confusion) {
  tp <- confusion$tp
  fn <- confusion$fn
  ifelse(tp + fn != 0, tp/(tp + fn), 1)
}

#' Specificity
#' 
#' Computes specificity from a confusion matrix, see \link{confusion}. 
#' Specificity is defined as TN/(TN + FP), where TN are true negatives and 
#' FP are false positives. If TN + FP = 0, 0 is returned. 
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
specificity <- function(confusion) {
  tn <- confusion$tn
  fp <- confusion$fp
  ifelse(tn + fp != 0, tn/(tn + fp), 1)
}

#' False Omission Rate
#' 
#' Computes false omission rate from a confusion matrix, see \link{confusion}. 
#' False omission rate is defined as FN/(FN + TN), where FN are false negatives and
#' TN are true negatives. If FN + TN = 0, 0 is returned. 
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
FOR <- function(confusion) {
  fn <- confusion$fn
  tn <- confusion$tn
  ifelse(fn + tn != 0, fn/(fn + tn), 1)
}

#' False Discovery Rate
#' 
#' Computes false discovery rate from a confusion matrix, see \link{confusion}. 
#' False discovery rate is defined as FP/(FP + TP), where FP are false positives and 
#' TP are true positives. If FP + TP = 0, 0 is returned. 
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
FDR <- function(confusion) {
  fp <- confusion$fp
  tp <- confusion$tp
  ifelse(fp + tp != 0, fp/(fp + tp), 1)
}

#' Negative predictive value
#' 
#' Computes negative predictive value recall from a confusion matrix, see \link{confusion}. 
#' Negative predictive value is defined as TN/(TN + FN), where TN are true negatives and 
#' FN are false negatives. If TP + FN = 0, 0 is returned. 
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
NPV <- function(confusion) {
  tn <- confusion$tn
  fn <- confusion$fn
  ifelse(tn + fn != 0, tn/(tn + fn), 1)
}

#' F1 score
#' 
#' Computes F1 score from a confusion matrix, see \link{confusion}. 
#' The F1 score is defined as  \eqn{2 * TP/(2 * TP + FP + FN)}, where TP are true positives,
#' FP are false positives, and FN are false negatives. If TP + FP + FN = 0, 1 is returned. 
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
F1 <- function(confusion) {
  tp <- confusion$tp
  fp <- confusion$fp
  fn <- confusion$fn
  ifelse(tp + fp + fn != 0, 2*tp/(2*tp + fp + fn), 1)
}

#' G1 score
#' 
#' Computes G1 score from a confusion matrix, see \link{confusion}. G1 score is F1 score with
#' reversed roles of 0/1 classifications, see Petersen et al. 2022. 
#' The G1 score is defined as  \eqn{2 * TN/(2 * TN + FN + FP)}, where TN are true negatives,
#' FP are false positives, and FN are false negatives. If TN + FN + FP = 0, 1 is returned. 
#' 
#' @references Petersen, Anne Helby, et al. "Causal discovery for observational sciences using 
#' supervised machine learning." arXiv preprint arXiv:2202.12813 (2022).
#' 
#' @inheritParams precision
#' 
#' @return A numeric in \[0,1\]. 
#' 
#' @export
G1 <- function(confusion) {
  tn <- confusion$tn
  fn <- confusion$fn
  fp <- confusion$fp
  ifelse(tn + fn + fp != 0, 2*tn/(2*tn + fn + fp), 1)
}



#' Structural hamming distance between adjacency matrices
#' 
#' Computes the structural hamming distance between two adjacency matrices. This implementation 
#' is a modification of the \code{\link[pcalg]{shd}} function from the pcalg package, but here we
#' avoid working on the heavy \code{graphNEL} objects for representing graphs that are used in the 
#' pcalg package. 
#' 
#' Note that the function is symmetric in the two inputted adjacency matrices. 
#' 
#' @param est_amat Estimated adjacency matrix
#' @param true_amat True adjacency matrix
#' 
#' @return A numeric (a non-negative integer).
#' 
#'@export
shd <- function(est_amat, true_amat) {
  m1 <- est_amat
  m2 <- true_amat
  shd <- 0
  s1 <- m1 + t(m1)
  s2 <- m2 + t(m2)
  s1[s1 == 2] <- 1
  s2[s2 == 2] <- 1
  ds <- s1 - s2
  ind <- which(ds > 0)
  m1[ind] <- 0
  shd <- shd + length(ind)/2
  ind <- which(ds < 0)
  m1[ind] <- m2[ind]
  shd <- shd + length(ind)/2
  d <- abs(m1 - m2)
  shd + sum((d + t(d)) > 0)/2
}


# shd.tamat <- function(est_amat, true_amat) {
#   shd.default(est_amat$amat, true_amat)
# }
# 
# 
# shd.tpdag <- function(est_amat, true_amat) {
#   shd.default(est_amat$amat, true_amat)
# }
