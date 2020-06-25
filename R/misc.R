
edgesFromAdjMat <- function(amat) {
  vnames <- rownames(amat)
  nvar <- dim(amat)[1]
  mat <- matrix(amat, nrow = nvar, ncol = nvar)
  outF <- data.frame(from = NULL, to = NULL)
  # browser()
  for (i in 1:nvar) {
    thisC <- mat[, i]
    if (sum(thisC) > 0) {
      outF <- rbind(outF, data.frame(from = vnames[i], to = vnames[as.logical(thisC)]))
    }
  }
  outF$from <- as.character(outF$from)
  outF$to <- as.character(outF$to)
  outF
}

#' @import pcalg
amat <- function(pcres) {
  as(pcres, "amat")
}
