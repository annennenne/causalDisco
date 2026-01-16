###############################################################################################################
# Miscellaneous small exported functions ######################################################################
###############################################################################################################

#' Convert adjacency matrix to graphNEL object
#' 
#' @param amat An adjacency matrix
#' 
#' @return A \code{graphNEL} object, see  \code{\link[graph]{graphNEL-class}}.
#' 
#' @importFrom methods as
#' 
#' @export
as.graphNEL <- function(amat) {
  thisClass <- class(amat)
  if ("tamat" %in% thisClass) {
    class(amat) <- "matrix"
  }
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
#' @param toFrom Logical indicating whether the resulting adjancency
#' matrix is "to-from" (default), or "from-to", see details. 
#' @param type  The type of adjancency matrix, must be one of \code{"pdag"} or 
#' \code{"ag"}. \code{"pdag"} should be used for directed graphs, namely
#'  DAG, CPDAG, MPDAG, TPDAG and PDAG adjacency matrices, i.e. adjacency matrices 
#'  where A(i,j) = A(j,i) = 1 is interpreted as an undirected edge. \code{"ag"}
#'  may be used for ADMGs, MAGs, PAGs and TPAGs, where further possible arrowhead
#'  options are available (see \link{amat})
#' 
#' @details 
#' A "to-from" \code{pdag} adjacency matrix is encoded as follows: A(i,j) = 1 and A(j,i) = 0 
#' means there is an edge i -> j. A(j,i) = 1 and A(i,j) = 0 means there is an edge j -> i. 
#' A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j. 
#' A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j. 
#' 
#' A "from-to" adjacency matrix is the transpose of a "to-from" adjacency matrix. 
#' A "from-to" \code{pdag} adjacency matrix is hence encoded as follows: A(i,j) = 1 and A(j,i) = 0 
#' means there is an edge j -> i. A(j,i) = 1 and A(i,j) = 0 means there is an edge i -> j. 
#' A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j. 
#' A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j. 
#' 
#' See \link{amat} for details about how an \code{ag} adjacency matrix is encoded. 
#' 
#' @importFrom methods as
#'  
#' @export
graph2amat <- function(graph, toFrom = TRUE, type = "pdag") {
  res <- as(graph, "matrix")
  if (toFrom) res <- t(res)
  attr(res, "tamat_type") <- type
  res
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

##' @importClassesFrom pcalg fciAlgo
#amat2fciAlgo <- function(amat) {
#  class(amat) <- c(class(amat), "matrix")
#  new("fciAlgo", amat = amat, call = NA, n = NA,
#      max.ord = NA,
#      max.ordPDSEP = NA,
#      n.edgetests = NA, n.edgetestsPDSEP = NA,
#      sepset = NA, pMax = NA, allPdsep = NA)
#  
#}

