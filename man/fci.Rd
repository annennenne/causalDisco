% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fci.R
\name{fci}
\alias{fci}
\title{Perform causal discovery using the FCI algorithm}
\usage{
fci(
  data = NULL,
  sparsity = 10^(-1),
  test = regTest,
  suffStat = NULL,
  method = "stable.fast",
  methodNA = "none",
  methodOri = "conservative",
  output = "pag",
  varnames = NULL,
  ...
)
}
\arguments{
\item{data}{A data.frame with data. All variables should be
assigned to exactly one period by prefixing them with the period name
(see example below).}

\item{sparsity}{The sparsity level to be used for independence
testing (i.e. significance level threshold to use for each test).}

\item{test}{A procedure for testing conditional independence.
The default, \code{regTest} uses a regression-based information
loss test. Another available option is \code{corTest} which
tests for vanishing partial correlations. User supplied functions
may also be used, see details below about the required syntax.}

\item{suffStat}{Sufficient statistic. If this argument is supplied, the
sufficient statistic is not computed from the inputted data. The format and
contents of the sufficient statistic depends on which test is being used.}

\item{method}{Which method to use for skeleton construction, must be
\code{"stable"}, \code{"original"}, or \code{"stable.fast"} (the default).
See \code{\link[pcalg]{skeleton}} for details.}

\item{methodNA}{Method for handling missing information (\code{NA} values).
Must be one of \code{"none"} (default, an error is thrown if \code{NA}s
are present), \code{"cc"} (complete case analysis, deletes all observations
that have any code{NA} values), or \code{"twd"} (test wise deletion, omits
observations with missing information test-by-test) (further details below).}

\item{methodOri}{Method for handling conflicting separating sets when orienting
edges, must be one of \code{"standard"}, \code{"conservative"} (the default) or
\code{"maj.rule"}. See \link[pcalg]{pc} for further details.}

\item{output}{One of \code{"pag"} or \code{"fciAlgo"}. If \code{"pag"}
a partial ancestral graph (PAG) object is outputted.
If \code{"fciAlgo"} the PAG is outputted as the
object class \code{\link[pcalg]{fciAlgo-class}} from the pcalg package. This is
intended for compatability with tools from that package.}

\item{varnames}{A character vector of variable names. It only needs to be supplied
if the \code{data} argument is not used, and data are hence passed exclusively
through the \code{suffStat} argument.}

\item{...}{Further optional arguments which are passed to
\code{\link[pcalg]{skeleton}} for the skeleton constructing phase.}
}
\description{
This is a wrapper function for the \code{\link[pcalg]{fci}} function as
implemented in the pcalg package. All computations are carried out by the
pcalg package. The default output object however matches that of \link{tfci}, see
this function for details about how the adjacency matrix is encoded.
}
\examples{
# simulate linear Gaussian data w unobserved variable L1
n <- 100
L1 <- rnorm(n) 
X1 <- rnorm(n)
X2 <- L1 + X1 + rnorm(n)
X3 <- X1 + rnorm(n)
X4 <- X3 + L1 + rnorm(n)
d <- data.frame(p1_X1 = X1,
                p1_X2 = X2,
                p2_X3 = X3,
                p2_X4 = X4)

# use FCI algorithm to recover PAG                
fci(d, test = corTest)


}
