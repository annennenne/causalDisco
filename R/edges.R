#' List of edges in adjacency matrix
#' 
#' Produces a list of edges from an adjacency matrix. 
#' 
#' @return A list consisting of two lists: One for oriented edges (\code{$dir}), 
#' and one for unoriented edges (\code{$undir}).
#' 
#' @param amat An adjacency matrix. 
#'
#'@export
edges <- function(amat) {
  p <- nrow(amat)
  edgeL <- lapply(split(amat, rep(1:p, each = p)), function(x) which(x == 1))
  
  out <- list()
  for (i in 1:p) {
    children <- edgeL[[i]]
    nchild <- length(children) 
    if (nchild > 0) {
      for (j in 1:nchild) {
        out <- c(out, list(c(i, children[j])))
      }
    }
  }
  oneway <- list()
  bothways <- list()
  if (length(out) > 0) {
    revout <- lapply(out, rev) 
    bothways <- base::intersect(revout, out)
    if (length(bothways) > 0) {
      oneway <-  base::setdiff(out, bothways)
      bothways <- unique(lapply(bothways, sort))
    } else{
      oneway <- out
    }
  }
  list(`dir` = oneway, `undir` = bothways)
}



##

