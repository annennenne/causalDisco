#' Perform causal discovery using the temporal FCI algorithm (TFCI)
#'
#' Use a modification of the FCI algorithm that makes use of background knowledge
#' in the format of a partial ordering. This may for instance come about when 
#' variables can be assigned to distinct tiers or periods (i.e., a temporal ordering). 
#' 
#' @details
#' The temporal/tiered background information enters several places in the TFCI 
#' algorithm: 1) In the skeleton construction phase, when looking for separating 
#' sets Z between two variables X and Y, Z is not allowed to contain variables that
#' are strictly after both X and Y in the temporal order. 2) This also applies to
#' the subsequent phase where the algorithm searches for possible D-SEP sets. 3)
#' Prior to other orientation steps, any cross-tier edges get an arrowhead placed 
#' at their latest node. 
#' 
#' After this, the usual FCI orientation rules are applied, see \link[pcalg]{udag2pag}
#' for details. 
#' 
#' @param methodOri Method for handling conflicting separating sets when orienting
#' edges, must be one of \code{"standard"}, \code{"conservative"} (the default) or 
#' \code{"maj.rule"}. See \link[pcalg]{pc} for further details. 
#' 
#' @author Anne Helby Petersen, Qixiang Chen, and Daniel Malinsky.
#' 
#' @return The default output is a \code{tpag} object. This is an
#' S3 object, i.e., a list, with entries: \code{$tamat} (the estimated adjacency 
#' matrix), \code{$order} (character vector with the order, as inputted to
#' this function), \code{$psi} (the significance level used for testing), and
#' \code{$ntests} (the number of tests conducted).  
#' 
#' The adjacency matrix A has four possible entry values: 0 (no edge), 1 (circle),
#' 2 (arrowhead), 3 (tail). It is encoded as a "to-from" adjacency matrix, which means
#' that e.g. A(i,j) = 1 places a circle in the directed from j to i. For example, if 
#' A(i,j) = 1 and A(j,i) = 2, we have that i o-> j. Similarly, A(i,j) = 2 and A(j,i) = 3 
#' mean that i <- j. Note that this is a transposed version of the adjacency 
#' matrix outputted for \code{fciAlgo} objects from the \code{pcalg} package, which
#' is "to-from". But it is consistent with the "from-to" adjacency matrices used
#' for \code{pcAlgo} objects from the same package. 
#' 
#'
#' @inheritParams tpc
#'
#' @examples
#' # simulate linear Gaussian data w unobserved variable L1
#' set.seed(123)
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
#' # use tfci algorithm to recover tpag (conservative edge orientation)
#' tfci(d, test = corTest, order = c("p1", "p2"))
#' 
#' # use tfci with standard (non-conservative) method for edge orientation
#' tfci(d, test = corTest, order = c("p1", "p2"), methodOri = "standard")
#' 
#' @include tpc.R
#' 
#' @importFrom pcalg pdsep skeleton pc.cons.intern
#' @importFrom stats na.omit
#' @importClassesFrom pcalg pcAlgo
#' 
#' @export
tfci <- function(data = NULL, order, sparsity = 10^(-1), test = regTest,
                 suffStat = NULL, method = "stable.fast",
                 methodNA = "none",
                 methodOri = "conservative",
                 varnames = NULL, ...) {
 # warning("TFCI is in alpha testing stage! Use at your own risk!")
  #check arguments
  #if (!output %in% c("tpag", "fciAlgo")) {
  #  stop("Output must be tpag or fciAlgo.")
  #}
  if (!(methodNA %in% c("none", "cc", "twd"))) {
    stop("Invalid choice of method for handling NA values.")
  }
  if (is.null(data) & is.null(suffStat)) {
    stop("Either data or sufficient statistic must be supplied.")
  }
  
  if (!(methodOri %in% c("standard", "conservative", "maj.rule"))) {
    stop("Orientation method must be one of standard, conservative or maj.rule.")
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
  
  #handle orientation method argument
  conservative <- FALSE
  maj.rule <- FALSE
  if (methodOri == "conservative") conservative <- TRUE
  if (methodOri == "maj.rule") maj.rule <- TRUE
  
  #variable names
  if (is.null(data)) {
    vnames <- varnames
  } else {
    vnames <- names(data)
  }
  
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
  
  # Do pdsep step: Adds further adjacencies to skeleton so that it corresponds
  # to MAG/PAG rather than DAG/CPDAG.  
  nvar <- length(skel@graph@nodes)
  
  # Consider allowing user to specify further arguments. However not possible
  # to pass arbitary arguments as pdsep doesn't have a ... argument. 
  fci_skel <- pdsep(skel = skel, suffStat = thisSuffStat, 
                    indepTest = thisDirTest, 
                    p = nvar,
                    sepset = skel@sepset,
                    pMax = skel@pMax,
                    unfVect = c(), 
                    alpha = sparsity)
  
  nextratests <- fci_skel$n.edgetests
  ntests <- ntests + nextratests
  
  unfVect <- NULL
  #browser()
  #if conservative or majority rule used for orientation
  if (conservative || maj.rule) {
    tmp <- new("pcAlgo", graph = as.graphNEL(t(fci_skel$G)),
               call = skel@call,
               n = integer(0), max.ord = as.integer(fci_skel$max.ord),
               n.edgetests = nextratests,
               sepset = fci_skel$sepset,
               pMax = fci_skel$pMax, 
               zMin = matrix(NA, 1, 1))
    tmpres <- pc.cons.intern(tmp, suffStat = thisSuffStat, 
                          indepTest = thisDirTest, 
                          alpha = sparsity,
                          version.unf = c(1, 1),
                          maj.rule = maj.rule)
    unfVect <- tmpres$unfTripl
    fci_skel$sepset <- tmpres$sk@sepset
  }
  
  
  # Case: Output tskeleton. Note: fci_skel adjmat is boolean, add 0 to 
  # to convert to numeric. Seems like amat
  # is not updated by pdsep function, instead, additional information
  # is added to the object. 
#  if (output == "tskeleton") {
 #   out <- list(tamat = tamat(amat = fci_skel$G + 0, order = order), 
#                psi = sparsity,
#                ntest = ntests)
#    class(out) <- "tskeleton"
#  } else { #case: output == "tpag"
    
  #Direct edges
  res <- tpag(fci_skel, order = order, unfVect = unfVect)
    
  #Pack up output
  #if (output == "tpag") {
    out <- list(tamat = tamat(amat = t(res), order = order, type = "ag"), 
                psi = sparsity) #,
#                ntests = ntests)
    class(out) <- "tpag"
  #} #else if (output == "fciAlgo") {
    #out <- res
  #}
  #}
  
  out
}


############################################################################
## Not exported below ######################################################
############################################################################


# Consider: Can sepsets ever contain something that breaks temporal
# ordering? Do not think so; such a sepset shouldn't be able to 
# come from dirTesting. Would have to enter in a different way
#' @importFrom pcalg udag2pag
tpag <- function(skel, order, unfVect, cautious = TRUE) {
  #boolean amat -> add 0 converts to numeric
  amat <- orderRestrictPAGSkel(skel$G + 0, order = order)
  sepsets <-  skel$sepset 
  
  # orientation rules to use
  # skips rules 5-7 (used for when selection bias is handled)
  userules <- rep(TRUE, 10)
  userules[5:7] <- FALSE
  
  if (cautious) sepsets <- orderRestrictSepset(sepsets, order,
                                               rownames(skel$G))
  
  #CHECK: Any rules need to be modifed or skipped?
  udag2pag(amat, sepset = sepsets, rules = userules,
           unfVect = unfVect)
}



#[i,j] is [from, to] for pag amats from pcalg package
# 0: no edge
# 1: circle
# 2: arrowhead ->
# 3: tail
orderRestrictPAGSkel <- function(amat, order) {
  p <- nrow(amat)
  vnames <- rownames(amat)
  
  for (i in 1:p) {
    for (j in 1:p) {
      if (amat[j,i] != 0 && is.after(vnames[i], vnames[j], order)) {
        amat[j, i] <- 2
        #don't have to change [i,j] entry - this will
        #be 1, which is circle because we are looking
        #at skeleton
      } 
        
        #&& amat[i,j] %in% c(1,2)) {
        #amat[j,i] <- 
      #}
    }
  }
  
  amat
}


orderRestrictSepset <- function(sepset, order, vnames) {
  p <- length(vnames)
  
  for (i in 1:p) {
    for (j in 1:p) {
      thisSS <- sepset[[i]][[j]]
      l <- length(thisSS)
      # thisSS can be vector, NULL or empty numeric
      
      if (l > 0) {
        problem <- FALSE
        for (k in 1:l) {
          if (is.after(vnames[thisSS[k]], vnames[i], order) &&
              is.after(vnames[thisSS[k]], vnames[j], order)) {
            sepset[[i]][[j]] <- NULL
            warning("Found sepset that was not allowed due to temporal order!")
          }
        }
      }
    }
  }
  sepset
}
