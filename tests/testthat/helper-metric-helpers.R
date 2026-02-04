amat <- function(n, edges = list()) {
  m <- matrix(0, n, n)
  for (e in edges) {
    m[e[1], e[2]] <- 1
  }
  m
}
