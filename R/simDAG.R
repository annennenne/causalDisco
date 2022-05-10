#' Simulate a random DAG
#' 
#' Simulates a random directed acyclic graph adjacency (DAG) matrix with 
#' the provided edge sparsity. The edge sparsity is the percentage of edges
#' that are absent, relative to a fully connected DAG. 
#' 
#' @param p The number of nodes.
#' @param sparsity If \code{NULL} (the default), a random edge sparsity
#' is sampled from the interval provided in \code{sparsityLim}. Otherwise,
#' the sparsity should be provided as a numeric in \[0,1\].  
#' @param sparsityLim A vector of two numerics, both must be in \[0,1\]. 
#' @param permute If \code{FALSE}, the adjacency matrix will include nodes
#' in their causal ordering. This is avoided by setting \code{permute = TRUE}, 
#' in which case the node order is permuted randomly.
#' 
#' @return An adjacency matrix. 
#'  
#' @examples 
#' # Simulate a DAG adjacency matrix with 5 nodes
#' simDAG(5) 
#' 
#' @importFrom stats runif rnorm sd
#' 
#' @export
simDAG <- function(p, sparsity = NULL,
                   sparsityLim = c(0, 0.8),
                   permute = TRUE) {
  if (is.null(sparsity)) {
    sparsity <- runif(1, min = sparsityLim[1], 
                      max = sparsityLim[2])
  }
  
  adjm <- matrix(1, p, p)
  adjm[upper.tri(adjm, diag = TRUE)] <- 0
  adjm_lowtri <- which(lower.tri(adjm))
  adjm[sample(adjm_lowtri, round(sparsity * length(adjm_lowtri)))] <- 0

  if (permute) {
    perm <- sample.int(p, replace = FALSE)
    adjm <- adjm[perm, perm]
  }
  
  rownames(adjm) <- colnames(adjm) <- paste("x", 1:p, sep = "")
  adjm
} 