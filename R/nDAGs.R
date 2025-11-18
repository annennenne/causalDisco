#' Number of different DAGs
#'
#' Computes the number of different possible DAGs that can be constructed over
#' a given number of nodes.
#'
#' @param p The number of nodes.
#'
#' @return A numeric.
#'
#' @export
nDAGs <- function(p) {
  if (p <= 1) {
    return(1)
  }

  dp <- numeric(p + 1)
  dp[1] <- 1 # nDAGs(0) is 1
  dp[2] <- 1 # nDAGs(1) is 1

  # Compute from 2..p
  for (k in 2:p) {
    i <- 1:k
    terms <- (-1)^(i + 1) * choose(k, i) * 2^(i * (k - i)) * dp[k - i + 1]
    dp[k + 1] <- sum(terms)
  }

  dp[p + 1]
}
