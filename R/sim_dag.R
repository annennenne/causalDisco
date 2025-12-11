#' Simulate a random DAG
#'
#' Simulates a random directed acyclic graph adjacency (DAG) matrix with
#' the provided edge sparsity. The edge sparsity is the percentage of edges
#' that are absent, relative to a fully connected DAG.
#'
#' @param p The number of nodes.
#' @param sparsity A numeric in \[0,1\] giving the proportion of possible
#' edges that should be removed. If \code{sparsity} is supplied, the value in
#' \code{sparsityLim} is ignored.
#' @param sparsityLim A numeric vector of length 2 giving the interval from
#' which a sparsity value is drawn when \code{sparsity = NULL}. Both values
#' must lie in \[0,1\].
#' @param permute If \code{FALSE}, the adjacency matrix will include nodes
#' in their causal ordering. This is avoided by setting \code{permute = TRUE},
#' in which case the node order is permuted randomly.
#'
#' @return An adjacency matrix.
#'
#' @examples
#' # Simulate a DAG adjacency matrix with 5 nodes and sparsity 0.5
#' sim_dag(5, sparsity = 0.5)
#'
#' @export
sim_dag <- function(p, sparsity = NULL,
                    sparsityLim = c(0, 0.8),
                    permute = TRUE) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "stats"
    ),
    function_name = "sim_dag"
  )

  if (is.null(sparsity)) {
    sparsity <- stats::runif(1,
      min = sparsityLim[1],
      max = sparsityLim[2]
    )
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
