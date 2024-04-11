#' Perform causal discovery using the FCI algorithm 
#' 
#' @description This is a wrapper function for the \code{\link[pcalg]{fci}} function as
#' implemented in the pcalg package. All computations are carried out by the 
#' pcalg package. The default output object however matches that of \link{tfci}, see
#' this function for details about how the adjacency matrix is encoded.
#'
#' @inheritParams tfci
#' @param output One of \code{"pag"} or \code{"fciAlgo"}. If \code{"pag"} 
#' a partial ancestral graph (PAG) object is outputted. 
#' If \code{"fciAlgo"} the PAG is outputted as the 
#' object class \code{\link[pcalg]{fciAlgo-class}} from the pcalg package. This is
#' intended for compatability with tools from that package. 
#'
#' @include tpc.R
#' 
#' @importFrom pcalg pdsep skeleton
#' @importFrom stats na.omit
#' 
#' @examples
#' # simulate linear Gaussian data w unobserved variable L1
#' n <- 100
#' L1 <- rnorm(n) 
#' X1 <- rnorm(n)
#' X2 <- L1 + X1 + rnorm(n)
#' X3 <- X1 + rnorm(n)
#' X4 <- X3 + L1 + rnorm(n)
#' d <- data.frame(p1_X1 = X1,
#'                 p1_X2 = X2,
#'                 p2_X3 = X3,
#'                 p2_X4 = X4)
#' 
#' # use FCI algorithm to recover PAG                
#' fci(d, test = corTest)
#' 
#' 
#' @export
fci <- function(data = NULL, sparsity = 10^(-1), test = regTest,
                 suffStat = NULL, method = "stable.fast",
                 methodNA = "none",
                 methodOri = "conservative",
                 output = "pag", 
                 varnames = NULL, ...) {
  
  #check arguments
  if (!output %in% c("pag", "fciAlgo")) {
    stop("Output must be pag or fciAlgo.")
  }
  if (!methodNA %in% c("none", "cc", "twd")) {
    stop("Invalid choice of method for handling NA values.")
  }
  if (is.null(data) & is.null(suffStat)) {
    stop("Either data or sufficient statistic must be supplied.")
  }
  if (!(methodOri %in% c("standard", "conservative", "maj.rule"))) {
    stop("Orientation method must be one of standard, conservative or maj.rule.")
  }
  
  
  #handle orientation method argument
  conservative <- FALSE
  maj.rule <- FALSE
  if (methodOri == "conservative") conservative <- TRUE
  if (methodOri == "maj.rule") maj.rule <- TRUE
  
  # handle missing information
  # note: twd is handled by the test: they have this as default, so the code here
  # is used to ensure that missing info is only passed along if we in fact want to 
  # use twd
  if (any(is.na(data))) {
    if (methodNA == "none") {
      stop("Inputted data contain NA values, but no method for handling missing NAs was supplied.")
    } else if (methodNA == "cc") {
      data <- na.omit(data)
      if (nrow(data) == 0) {
        stop("Complete case analysis chosen, but inputted data contain no complete cases.")
      }  
    }
  }
  
  #variable names
  if (is.null(data)) {
    vnames <- varnames
  } else {
    vnames <- names(data)
  }
  
  
  #Construct sufficient statistic for built-in tests
  if (is.null(suffStat)) {
    thisTestName <- deparse(substitute(test))
    if (thisTestName == "regTest") {
      thisSuffStat <- makeSuffStat(data, type = "regTest")
    } else if (thisTestName == "corTest") {
      thisSuffStat <- makeSuffStat(data, type = "corTest")
    } else {
      stop(paste("suffStat needs to be supplied",
                 "when using a non-builtin test."))
    }
  } else {
    thisSuffStat <- suffStat
    methodNA <- "none" #can't handle NA for user-supplied suff. stat./test
  }
  
  # do fci
  res <- pcalg::fci(suffStat = thisSuffStat,
                    indepTest = test,
                    alpha = sparsity,
                    labels = vnames,
                    skel.method = method,
                    conservative = conservative,
                    maj.rule = maj.rule,
                    ...)
  
  
  ntests <- sum(res@n.edgetests)
  
  #Pack up output
  if (output == "pag") {
    out <- list(amat = graph2amat(res, type = "ag"), psi = sparsity) #,
#                ntests = ntests)
    class(out) <- "pag"
  } else if (output == "fciAlgo") {
    out <- res
  }
  
  out
}

