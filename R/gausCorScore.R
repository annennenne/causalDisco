#' Gaussian L0 score computed on correlation matrix
#' 
#' The score is intended to be used with score-based causal discovery algorithms
#' from the pcalg package. It is identical to the \code{\link[pcalg]{GaussL0penObsScore-class}}, 
#' except that it takes in a correlation matrix instead of the full data set. 
#' \code{\link[pcalg]{GaussL0penObsScore-class}}.
#' 
#' 
#' @param cormat A correlation matrix. Needs to be symmetric.  
#' @param n The number of observations in the dataset that the correlation matrix was computed from.
#' @param p The number of variables. This is inferred from the cormat if not supplied.
#' @param lambda Penalty to use for the score. If \code{NULL} (default), the BIC score penalty is used. See
#' \code{\link[pcalg]{GaussL0penObsScore-class}} for further details. 
#' @param ... Other arguments passed along to \code{\link[pcalg]{GaussL0penObsScore-class}}. 
#' 
#' @return A \code{Score} object (S4), see \code{\link[pcalg]{Score-class}}.
#' 
#' @importFrom methods new
#' 
#' @examples 
#' # Simulate data and compute correlation matrix
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' x3 <- x1 + x2 + rnorm(100)
#' d <- data.frame(x1, x2, x3)
#' cmat <- cor(d)
#' 
#' # Use gausCorScore with pcalg::ges() 
#' pcalg::ges(gausCorScore(cmat, n = 100))
#' 
#' 
#' @export 
gausCorScore <- function(cormat, n, p = NULL, 
                         lambda = NULL, ...)  {
  if (is.null(lambda)) lambda <- log(n)/2
  
  if (is.null(p)) p <- dim(cormat)[1]
  
  outscore <- new("GaussL0penObsScore", matrix(1,1,1),
                  lambda = lambda, intercept = FALSE, ...)
  
  #drop entries not needed 
  outscore$pp.dat$data <- NULL
  outscore$pp.dat$non.int <- NULL
  outscore$pp.dat$target.index <- NULL
  
  #fill in entries with custom calculations
  outscore$pp.dat$vertex.count <- p
  outscore$pp.dat$data.count <- rep(n, p)
  outscore$pp.dat$total.data.count <- n
  outscore$pp.dat$scatter.index <- rep(1, p)
  outscore$pp.dat$scatter<- list(cbind(rbind(cormat, rep(0, p)), c(rep(0, p),1)) * (n - 1))
  outscore$.nodes <- rownames(cormat)
  
  outscore
}

