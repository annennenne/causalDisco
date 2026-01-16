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
  if (p <= 1) return(1)
  else {
    terms <- numeric(p)
    for (i in 1:p) {
      terms[i] <- (-1)^(i + 1) * choose(p, i) * 2^(i * (p - i)) * nDAGs(p - i)
    }
    sum(terms)
  }
}
