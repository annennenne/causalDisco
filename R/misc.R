###############################################################################################################
# Miscellanous small exported functions #######################################################################
###############################################################################################################

#' Convert adjacency matrix to graphNEL object
#' 
#' @param amat An adjacency matrix
#' 
#' @return A \code{graphNEL} object, see  \code{\link[graph]{graphNEL-class}}.
#' 
#' @export
as.graphNEL <- function(amat) {
  as(t(amat), "graphNEL")
}

#' Check for PDAG
#' 
#' @details Check: Is adjacency matrix proper PDAG? See \code{\link[pcalg]{isValidGraph}} for 
#' definition. 
#' 
#' @param amat An adjacency matrix
#' 
#' @return A logical.
#' 
#' @export
is_pdag <- function(amat) {
  pcalg::isValidGraph(amat, "pdag")
}

#' Check for CPDAG
#' 
#' @details Check: Is adjacency matrix proper CPDAG? See \code{\link[pcalg]{isValidGraph}} for 
#' definition. 
#' 
#' @param amat An adjacency matrix
#' 
#' @return A logical.
#' 
#' @export
is_cpdag <- function(amat) {
  pcalg::isValidGraph(amat, "cpdag")
}

#' Convert graphNEL object to adjacency matrix
#' 
#' @param graph A graphNEL object. 
#' 
#' @export
graph2amat <- function(graph) {
  t(as(graph, "matrix"))
}


#' Compute maximal number of edges for graph
#' 
#' Computes the number of edges a graph with \code{p} nodes will have if its 
#' fully connected. 
#' 
#' @param p The number of nodes in the graph
#' 
#' @return A numeric. 
#' 
#' @export 
maxnedges <- function(p) {
  sum(1:(p-1))
}


#' Convert essential graph to adjacency matrix
#' 
#' Extracts the adjacency matrix from an \code{\link[pcalg]{EssGraph-class}} object. This object is returned
#' by score-based causal discovery algorithms in the pcalg package.  
#' 
#' @param essgraph An \code{EssGraph} object
#' @param p The number of nodes in the graph
#' 
#' @return An adjacency matrix (square matrix with 0/1 entries).
#' 
#' @export 
essgraph2amat <- function(essgraph, p = length(essgraph$field(".nodes"))) {
  inlist <- essgraph$field(".in.edges")
  out <- t(sapply(inlist, which2indicator, p = p))
  colnames(out) <- rownames(out)
  out
}


#' Compute average degree for adjacency matrix
#' 
#' Computes the average degree, i.e. the number of edges divided 
#' by the number of nodes. 
#' 
#' @param amat An adjacency matrix 
#' 
#' @return A numeric. 
#' 
#' @export
average_degree <- function(amat) {
  p <- nrow(amat)
  sum(amat + t(amat) > 0)/p
}

#' Number of edges in adjacency matrix
#' 
#' Counts the number of edges in an adjacency matrix.
#' 
#' @param amat An adjacency matrix 
#' 
#' @return A numeric (non-negative integer). 
#' 
#' @export
nedges <- function(amat) {
  sum(halfskel(amat))
}



###############################################################################################################
# Not exported below###########################################################################################
###############################################################################################################


which2indicator <- function(x, p) {
  out <- rep(0, p)
  out[x] <- 1
  out
}


halfskel <- function(amat) {
  out <- amat + t(amat) != 0
  as.numeric(out[lower.tri(out)])
}
