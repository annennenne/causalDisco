#' Perform causal discovery using the FCI algorithm (pcalg backend)
#'
#' @description
#' Thin wrapper around \code{\link[pcalg]{fci}}. Computations are done by
#' \pkg{pcalg}; the default output mirrors \code{\link{tfci}}.
#'
#' @param data A data.frame of variables (column names become labels).
#' @param sparsity Significance threshold (\code{alpha}) for CI tests.
#' @param test Independence test function (e.g., \code{regTest}, \code{corTest}).
#' @param suffStat Sufficient statistics list (skips computing from \code{data}).
#' @param method Skeleton method: \code{"stable"}, \code{"original"},
#'   or \code{"stable.fast"} (default).
#' @param methodNA Missing‑data handling: \code{"none"}, \code{"cc"}, or \code{"twd"}.
#' @param methodOri Orientation strategy: \code{"standard"}, \code{"conservative"},
#'   or \code{"maj.rule"}.
#' @param output Either \code{"pag"} (default) or \code{"fciAlgo"}.
#' @param varnames Optional variable names (use when only \code{suffStat} is given).
#' @param ... Further arguments passed to \code{pcalg::fci()}.
#'
#' @return A PAG list (for \code{output = "pag"}) or a \code{pcalg::fciAlgo} object.
#' @export
pcalg_fci <- function(data = NULL, sparsity = 10^(-1), test = regTest,
                      suffStat = NULL, method = "stable.fast",
                      methodNA = "none",
                      methodOri = "conservative",
                      output = "pag",
                      varnames = NULL, ...) {
  # check arguments
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

  # do fci
  res <- pcalg::fci(
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
  if (output == "pag") {
    out <- list(amat = graph2amat(res, type = "ag"), psi = sparsity) # ,
    #                ntests = ntests)
    class(out) <- "pag"
  } else if (output == "fciAlgo") {
    out <- res
  }

  out
}
