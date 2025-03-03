% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tges.R
\docType{class}
\name{TemporalBIC-class}
\alias{TemporalBIC-class}
\title{Temporal Bayesian Information Criterion (Score criterion)}
\arguments{
\item{data}{A numeric matrix with \eqn{n} rows and \eqn{p} columns. Each row
corresponds to one observational realization.}

\item{order}{A vector specifying the order each variable. Can be either a vector of integers
or an vector of prefixes. If integers, such that the ith entry
will detail the order of the ith variable in the dataset. Must start at 1 an increase
with increments of 1. If prefixes, must be in order.}

\item{lambda}{Penalization constant (see details).}

\item{intercept}{Logical; indicates whether an intercept is allowed in the
linear structural equations (i.e., whether a nonzero mean is allowed).}
}
\description{
A Reference Class for Gaussian Observational Data Scoring with Tiered Background Knowledge. This class represents a score for causal discovery using tiered background knowledge from observational Gaussian
data; it is used in the causal discovery function \code{\link{tges}}.
}
\details{
The class implements a score which scores all edges contradicting the ordering
(edge going from a later tier to an earlier) to minus \eqn{\infty}{∞}. If the
the edges does not contradict, the score is equal to that of \code{\linkS4class{GaussL0penObsScore}}:
The class implements an \eqn{\ell_0}{ℓ0}-penalized Gaussian maximum
likelihood estimator. The penalization is a constant (specified by
the argument \code{lambda} in the constructor) times the number of
parameters of the DAG model. By default, the constant \eqn{\lambda}{λ} is
chosen as \eqn{\log(n)/2}{log(n)/2}, which corresponds to the BIC score.
}

\section{Extends}{

Class \code{\linkS4class{GaussL0penObsScore}} from \pkg{pcalg}, directly.

All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}

\section{Constructor}{

\preformatted{
new("TemporalBIC",
  data = matrix(1, 1, 1),
  order =  rep(1,ncol(data)),
  lambda = 0.5 * log(nrow(data)),
  intercept = TRUE,
  ...)
}
}

\examples{
#Simulate Gaussian data
set.seed(123)
n <- 500
child_x <- rnorm(n)
child_y <- 0.5*child_x + rnorm(n)
child_z <- 2*child_x + child_y  + rnorm(n)

adult_x <- child_x + rnorm(n)
adult_z <- child_z + rnorm(n)
adult_w <- 2*adult_z + rnorm(n)
adult_y <- 2*child_x + adult_w + rnorm(n)

simdata <- data.frame(child_x, child_y, child_z,
                     adult_x, adult_z, adult_w,
                      adult_y)

# Define order in prefix way
prefix_order <- c("child", "adult")

# Define TBIC score
t_score <- new("TemporalBIC", order = prefix_order
               , data = simdata)
# Run tges
tges_pre <- tges(t_score)

# Plot MPDAG
plot(tges_pre)

# Define order in integer way
integer_order <- c(1,1,1,2,2,2,2)

# Define TBIC score
t_score <- new("TemporalBIC", order = integer_order
               , data = simdata)
# Run tges
tges_int <- tges(t_score)

# Plot MPDAG
plot(tges_int)

}
\seealso{
\code{\link{tges}}
}
\author{
Tobias Ellegaard Larsen
}
