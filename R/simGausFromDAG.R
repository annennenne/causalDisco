#' Simulate Gaussian data according to DAG
#' 
#' Simulates a jointly Gaussian dataset given a DAG 
#' adjacency matrix ("from-to" encoding, see \link{amat} for details). 
#' The data is simulated using linear structural
#' equations and the parameters (residual standard deviations and 
#' regression coefficients) are sampled from chosen intervals. 
#' 
#' @details A variable \eqn{X_{i}} is simulated as \cr
#' \eqn{X_{i} := \sum_{Z \in pa(X_{i})} \beta_{Z} * Z + e_{i}} \cr
#' where \eqn{pa(X_{i})} are the parents of \eqn{X_{i}} in the DAG. 
#' The residual, \eqn{e_{i}}, is drawn from a normal distribution. 
#' 
#' @param amat An adjacency matrix. 
#' @param n The number of observations that should be simulated. 
#' @param regparLim The interval from which regression parameters are 
#' sampled. 
#' @param resSDLim The interval from which residual standard deviations 
#' are sampled. 
#' @param pnegRegpar The probability of sampling a negative regression
#' parameter.
#' @param standardize If \code{FALSE} (the default), the raw data are 
#' returned. If \code{TRUE}, the data are first standardized, i.e.,
#' each variable will have its mean subtracted and be divided by its
#' standard deviation. 
#' 
#' @return A data.frame of identically distributed simulated observations. 
#' 
#' @examples
#' # Simulate DAG adjacency matrix with 6 nodes
#' ex_dag <- simDAG(6) 
#' 
#' # Simulate Gaussian data (100 iid observations) 
#' ex_data <- simGausFromDAG(ex_dag, n = 100)
#' 
#' @export
simGausFromDAG <- function(amat, n, regparLim = c(0.5, 2), 
                           resSDLim = c(0.1, 1),
                           pnegRegpar = 0.4,
                           standardize = FALSE) {
  
  p <- nrow(amat)
  
  data <- matrix(0, n, p, dimnames = list(NULL, colnames(amat)))
  
  residualsds <- runif(p, min = resSDLim[1], max = resSDLim[2])
  
  #first col (always exogenous)
  data[, 1] <- rnorm(n, sd = residualsds[1])
  
  
  for (i in 2:p) {
    #regression parameters
    pars <- runif(p, min = regparLim[1], max = regparLim[2]) * 
      sample(c(-1, 1), p, replace = TRUE, prob = c(pnegRegpar, 1 -  pnegRegpar))
    usepars <- pars * amat[i, ]
    
    thisVar <- data %*% usepars + rnorm(n, sd = residualsds[i])
    
    if (standardize) thisVar <- (thisVar - mean(thisVar)) / sd(thisVar)
    
    data[, i] <- thisVar
  }
  
  as.data.frame(data)
}
