#' Perform causal discovery using the temporal PC algorithm (TPC)
#'
#' @param data A data.frame with data. All variables should be
#' assigned to exactly one period by prefixing them with the period name
#' (see example below).
#' @param order A character vector with period-prefixes in their
#' temporal order (see example below).
#' @param sparsity The sparsity level to be used for independence
#' testing (i.e. significance level threshold to use for each test).
#' @param test A procedure for testing conditional independence.
#' The default, \code{regTest} uses a regression-based information
#' loss test. Another available option is \code{corTest} which
#' tests for vanishing partial correlations. User supplied functions
#' may also be used, see details below about the required syntax.
#' @param suffStat Sufficient statistic. If this argument is supplied, the
#' sufficient statistic is not computed from the inputted data. The format and
#' contents of the sufficient statistic depends on which test is being used. 
#' @param method Which method to use for skeleton construction, must be 
#' \code{"stable"}, \code{"original"}, or \code{"stable.fast"} (the default).  
#' See \code{\link[pcalg]{skeleton}} for details. 
#' @param methodNA Method for handling missing information (\code{NA} values).
#' Must be one of \code{"none"} (default, an error is thrown if \code{NA}s 
#' are present), \code{"cc"} (complete case analysis, deletes all observations
#' that have any code{NA} values), or \code{"twd"} (test wise deletion, omits
#' observations with missing information test-by-test) (further details below). 
#' @param output One of \code{"tpdag"} or \code{"tskeleton"}. If
#' \code{"tskeleton"}, a temporal skeleton is constructed and outputted,
#' but the edges are not directed. If \code{"tpdag"} (the default), a
#' the edges are directed, resulting in a temporal partially directed
#' acyclic graph.
#' @param ... Further optional arguments which are passed to 
#' \code{\link[pcalg]{skeleton}} for the skeleton constructing phase.
#'
#' @details Note that all independence test procedures implemented
#' in the \code{pcalg} package may be used, see \code{\link[pcalg]{pc}}.
#' 
#' The methods for handling missing information require that the \code{data},
#' rather than the \code{suffStat} argument is used for inputting data; the latter
#' assumes no missing information and hence always sets \code{methodNA = "none"}.
#' If the test is \code{corTest}, test-wise deletion is performed when computing the 
#' sufficient statistic (correlation matrix) (so for each pair of variables, only 
#' complete cases are used). If the test is \code{regTest}, test-wise deletion 
#' is performed for each conditional independence test instead. 
#
#' @return A \code{tpdag} or \code{tskeleton} object. Both return types are
#' S3 objects, i.e., lists with entries: \code{$amat} (the estimated adjacency 
#' matrix), \code{$order} (character vector with the order, as inputted to
#' this function), \code{$psi} (the significance level used for testing), and
#' \code{$ntests} (the number of tests conducted). 
#'
#'
#'
#' @examples
#' #TPC on included example data, use sparsity psi = 0.01, default test (regression-based
#' #information loss):
#' data(tpcExample)
#' tpc(tpcExample, order = c("child", "youth", "oldage"), sparsity = 0.01)
#' 
#' 
#' #TPC on included example data, use sparsity psi = 0.01, use test for vanishing partial
#' # correlations:
#' data(tpcExample)
#' tpc(tpcExample, order = c("child", "youth", "oldage"), sparsity = 0.01,
#' test = corTest)
#' 
#'
#' #TPC on another simulated data set
#' 
#' #Simulate data
#' set.seed(123)
#' n <- 500
#' child_x <- rnorm(n)^2
#' child_y <- 0.5*child_x + rnorm(n)
#' child_z <- sample(c(0,1), n, replace = TRUE,
#'                   prob = c(0.3, 0.7))
#'          
#' adult_x <- child_x + rnorm(n)
#' adult_z <- as.numeric(child_z + rnorm(n) > 0)
#' adult_w <- 2*adult_z + rnorm(n)
#' adult_y <- 2*sqrt(child_x) + adult_w^2 + rnorm(n)
#'
#' simdata <- data.frame(child_x, child_y, child_z,
#'                       adult_x, adult_z, adult_w,
#'                       adult_y)
#'
#' #Define order
#' simorder <- c("child", "adult")
#'
#' #Perform TPC with sparsity psi = 0.001
#' results <- tpc(simdata, order = simorder, sparsity = 10^(-3))
#'
#' @importFrom pcalg skeleton
#'
#' @export
tpc <- function(data, order, sparsity = 10^(-1), test = regTest,
                suffStat = NULL, method = "stable.fast",
                methodNA = "none",
                output = "tpdag", ...) {

  #check arguments
  if (!output %in% c("tpdag", "tskeleton")) {
    stop("Output must be tpdag or tskeleton.")
  }
  if (!methodNA %in% c("none", "cc", "twd")) {
    stop("Invalid choice of method for handling NA values.")
  }
  
  
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
  vnames <- names(data)

  #make testing procedure that does not condition on
  #the future
  thisDirTest <- dirTest(test, vnames, order)

  #Construct sufficient statistic for built-in tests
  if (is.null(suffStat)) {
    #thisTestName <- deparse(match.call()[["test"]])
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

  #Learn skeleton
  skel <- skeleton(suffStat = thisSuffStat,
                   indepTest = thisDirTest,
                   alpha = sparsity,
                   labels = vnames,
                   method = method, ...)
  ntests <- sum(skel@n.edgetests)


  if (output == "tskeleton") {
    out <- list(tamat = tamat(amat = skel@amat, order = order), psi = sparsity,
                ntest = ntests)
    class(out) <- "tskeleton"
  } else { #case: output == "tpdag"

    #Direct edges
    res <- tpdag(skel, order = order)

    #Pack up output
    out <- list(tamat = tamat(amat = amat(res), order = order), psi = sparsity,
                ntests = ntests)
    class(out) <- "tpdag"
  }

  out
}





############################################################################
## Not exported below ######################################################
############################################################################





makeSuffStat <- function(data, type, ...) {
  #browser()
  if (type == "regTest") {
    bin <- unlist(sapply(data, function(x) length(unique(na.omit(x))) == 2))
    suff <- list(data = data, binary = bin)
  #  if (!is.null(order)) suff$order <- order
  } else if (type == "corTest") {
    suff <- list(C = stats::cor(data, use = "pairwise.complete.obs"), 
                 n = nrow(data))
  } else {
    stop(paste(type, "is not a supported type for",
               "autogenerating a sufficient statistic"))
  }

  #else suff <- list(data = data)
  #suff$otherArgs <- list(...)

  suff
}

#is x (strictly) after y in order?
is.after <- function(x, y, order, sep = "_") {
  prefix_x <- strsplit(x, sep)[[1]][1]
  prefix_y <- strsplit(y, sep)[[1]][1]
  res <- which(order == prefix_x) > which(order == prefix_y)
  if (length(res) == 0) res <- FALSE
  res
}




dirTest <- function(test, vnames, order) {
  thistest <- function(x, y, S, suffStat) {

    #check if we need to conduct the test at all
    #(whether S occurs strictly after both
    # x and y in order)
    snames <- vnames[S] #NOTE: CHECK IF THIS IS CORRECT FOR EMPTY S
    xname <- vnames[x]
    yname <- vnames[y]
    laterS <- FALSE
    i <- 1
    nS <- length(snames)
    while(!laterS & i <= nS) {
      s <- snames[i]
      afterx <- is.after(s, xname, order)
      aftery <- is.after(s, yname, order)
      if (afterx & aftery) laterS <- TRUE
      i <- i + 1
    }
    if (laterS) {
      return(0)
    }

    #If no order problem, use test function:
    do.call(test, list(x = x, y = y, S = S, suffStat = suffStat))
  }
  thistest
}


#' @importFrom pcalg addBgKnowledge
tpdag <- function(skel, order) {
  thisAmat <- amat(skel)

  #order restrict amat
  tempSkelAmat <- orderRestrictAmat(thisAmat, order = order)

  pcalg::addBgKnowledge(vOrientTemporal(tempSkelAmat, skel@sepset), checkInput = FALSE)
}



orderRestrictAmat <- function(amat, order) {
  p <- nrow(amat)
  vnames <- rownames(amat)

  for (i in 1:p) {
    for (j in 1:p) {
      if (is.after(vnames[i], vnames[j], order)) amat[j,i] <- 0
    }
  }
  amat
}




#' 
#' @importFrom gtools combinations
#' 
vOrientTemporal <- function(amat, sepsets) {
  vnames <- rownames(amat)
  nvar <- nrow(amat)

  for (i in 1:nvar) {
    theseAdj <- findAdjacencies(amat, i)

    #if there are at least two adjacent nodes
    if (length(theseAdj) >= 2) {

      adjpairs <- combinations(length(theseAdj), 2, v = theseAdj) #gtools

      npairs <- nrow(adjpairs)

      if (npairs >= 1) {

        for (j in 1:npairs) {
          thisPair <- adjpairs[j,]
          j1 <- thisPair[1]
          j2 <- thisPair[2]
          thisPairAdj <- j2 %in% findAdjacencies(amat, j1)

          #if pair is not adjacent (unmarried)
          if (!thisPairAdj) {

            sepset1 <- sepsets[[j1]][[j2]]
            sepset2 <- sepsets[[j2]][[j1]]

            #if middle node is not a separator of two other nodes
            if (!(i %in% sepset1) & !(i %in% sepset2)) {

              #if this does not contradict directional information
              #already in the graph
              if (amat[i,j1] == 1 & amat[i,j2] == 1) {
                amat[j1, i] <- 0
                amat[j2, i] <- 0
              }
            }
          }
        }
      }
    }
  }
  amat
}


findAdjacencies <- function(amatrix, index) {
  union(which(as.logical(amatrix[index,])), which(as.logical(amatrix[,index])))
}


edgesFromAdjMat <- function(amat) {
  vnames <- rownames(amat)
  nvar <- dim(amat)[1]
  mat <- matrix(amat, nrow = nvar, ncol = nvar)
  outF <- data.frame(from = NULL, to = NULL)
  # browser()
  for (i in 1:nvar) {
    thisC <- mat[, i]
    if (sum(thisC) > 0) {
      outF <- rbind(outF, data.frame(from = vnames[i], to = vnames[as.logical(thisC)]))
    }
  }
  outF$from <- as.character(outF$from)
  outF$to <- as.character(outF$to)
  outF
}


#' @importFrom methods as
amat <- function(pcres) {
  as(pcres, "amat") #methods
}


## Old function that may be useful if we want to add bnlearn engine 
## #' @importFrom dplyr intersect
## makeBgKnowledge <- function(amat, data, order, sep = "_") {
##  # browser()
##  crossTimeWL <- orderedBL(data, order = rev(order), sep = sep)
##  edges <- edgesFromAdjMat(amat)
##  fromPrefixes <- sapply(strsplit(edges$from, split = sep), function(x) x[1])
##  toPrefixes <- sapply(strsplit(edges$to, split = sep), function(x) x[1])
##  crossTimeEdges <- edges[fromPrefixes != toPrefixes,]
##  #edgesOut <- rbind(edges[fromPrefixes == toPrefixes,],
## #                  dplyr::intersect(crossTimeWL, crossTimeEdges))
##  #edgesOut
##  intersect(crossTimeWL, crossTimeEdges) #dplyr
##}


####

