#' Compare two tpdag or tskeleton objects
#'
#' Compare edges in two tpdag objects or two tskeleton objects.
#' Note that they should be based on the same variables.
#' Only edge absence/presence is compared, not edge orientation.
#'
#' @param x First object
#' @param y Second object (optional)
#'
#' @export
compare <- function(x, y = NULL) {
  if (is.null(y)) {
    y <- x
    onlyone <- TRUE
  } else onlyone <- FALSE
  if("tpdag" %in% class(x) & "tpdag" %in% class(y)) {
    amat1 <- x$amat
    amat2 <- y$amat
    psi1 <- x$psi
    psi2 <- y$psi

    edges1 <- as.numeric(amat1 + t(amat1) > 0)
    edges2 <- as.numeric(amat2 + t(amat2) > 0)
  } else if (is.numeric(x) & is.numeric(y) &&
             length(x) == length(y)) {
    psi1 <- NA
    psi2 <- NA
    edges1 <- x
    edges2 <- y
  } else stop("x and y must either both be tpdags or be numeric vectors of the same length.")


  #note: all numbers should be divided by 2 as
  #we are counting each edge twice (one time
  #for each direction)

  n1 <- sum(edges1)/2
  n2 <- sum(edges2)/2

  nadded <- sum(edges2 - edges1 == 1)/2
  nremoved <- sum(edges2 - edges1 == -1)/2

  if (onlyone) {
    nadded <- nremoved <- NA
  }

  out <- list(nedges1 = n1, nedges2 = n2,
              psi1 = psi1, psi2 = psi2,
              nadded = nadded,
              nremoved = nremoved)

  out
}


