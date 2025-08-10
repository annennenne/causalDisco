#' Perform causal discovery using the PC algorithm
#'
#' @param data A data.frame with data. All variables should be
#' assigned to exactly one period by prefixing them with the period name
#' (see example below).
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
#' that have any \code{NA} values), or \code{"twd"} (test wise deletion, omits
#' observations with missing information test-by-test) (further details below).
#' @param methodOri Method for handling conflicting separating sets when orienting
#' edges, must be one of \code{"standard"}, \code{"conservative"} (the default) or
#' \code{"maj.rule"}. See \link[pcalg]{pc} for further details.
#' @param output One of \code{"cpdag"}, \code{"skeleton"} or \code{"pcAlgo"}. If
#' \code{"skeleton"}, a skeleton is constructed and outputted,
#' but the edges are not directed. If \code{"cpdag"} (the default),
#' the edges are directed, resulting in a completed partially directed
#' acyclic graph (CPDAG). If \code{"pcAlgo"} the CPDAG is outputted as the
#' object class \code{\link[pcalg]{pcAlgo-class}} from the pcalg package. This is
#' intended for compatability with tools from that package.
#' @param varnames A character vector of variable names. It only needs to be supplied
#' if the \code{data} argument is not used, and data are hence passed exclusively
#' through the \code{suffStat} argument.
#' @param conservative Logital, if \code{TRUE} the conservative version of PC is used
#' (see \code{\link[pcalg]{pc}} for details).
#' @param ... Further optional arguments which are passed to
#' \code{\link[pcalg]{skeleton}} if \code{output = "skeleton"} or to
#' \code{\link[pcalg]{pc}} otherwise.
#'
#' @description This is a wrapper function for the \code{\link[pcalg]{pc}} function as
#' implemented in the pcalg package. All computations are carried out by the
#' pcalg package.
#'
#'
#' @details
#' Note that all independence test procedures implemented
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
#' \dontrun{
#' # PC on included example data, use sparsity psi = 0.01, default test (regression-based
#' # information loss):
#' data(tpcExample)
#' pc(tpcExample, sparsity = 0.01)
#'
#'
#' # PC on included example data, use sparsity psi = 0.01, use test for vanishing partial
#' # correlations:
#' data(tpcExample)
#' pc_pcalg_wrapper(tpcExample, sparsity = 0.01, test = corTest)
#' }
#' @importFrom pcalg skeleton
#' @importFrom stats na.omit
#'
#' @include tpc.R
#'
#' @export
pcalg_pc <- function(data = NULL, sparsity = 10^(-1), test = regTest,
                     suffStat = NULL, method = "stable.fast",
                     methodNA = "none",
                     methodOri = "conservative",
                     output = "cpdag",
                     varnames = NULL,
                     conservative = TRUE, ...) {
  # check arguments
  if (!output %in% c("cpdag", "skeleton", "pcAlgo")) {
    stop("Output must be cpdag, skeleton or pcAlgo.")
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


  # handle orientation method argument
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

  # variable names
  if (is.null(data)) {
    vnames <- varnames
  } else {
    vnames <- names(data)
  }

  # Construct sufficient statistic for built-in tests
  if (is.null(suffStat)) {
    thisTestName <- deparse(substitute(test))
    if (thisTestName == "regTest") {
      thisSuffStat <- makeSuffStat(data, type = "regTest")
    } else if (thisTestName == "corTest") {
      thisSuffStat <- makeSuffStat(data, type = "corTest")
    } else {
      stop(paste(
        "suffStat needs to be supplied",
        "when using a non-builtin test."
      ))
    }
  } else {
    thisSuffStat <- suffStat
    methodNA <- "none" # can't handle NA for user-supplied suff. stat./test
  }




  if (output == "skeleton") {
    # Learn skeleton
    skel <- skeleton(
      suffStat = thisSuffStat,
      indepTest = test,
      alpha = sparsity,
      labels = vnames,
      method = method, ...
    )
    ntests <- sum(skel@n.edgetests)
    out <- list(
      amat = graph2amat(skel), psi = sparsity,
      ntest = ntests
    )
    class(out) <- "skeleton"
  } else { # case: output == "cpdag" or "pcAlgo"

    # Direct edges
    res <- pcalg::pc(
      suffStat = thisSuffStat,
      indepTest = test,
      alpha = sparsity,
      labels = vnames,
      skel.method = method,
      conservative = conservative,
      maj.rule = maj.rule,
      ...
    )
    ntests <- sum(res@n.edgetests)

    # Pack up output
    if (output == "cpdag") {
      out <- list(
        amat = graph2amat(res, toFrom = FALSE), psi = sparsity,
        ntests = ntests
      )
      class(out) <- "cpdag"
    } else if (output == "pcAlgo") {
      out <- res
    }
  }

  out
}
