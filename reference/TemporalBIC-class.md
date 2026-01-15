# Temporal Bayesian Information Criterion (Score criterion)

A Reference Class for Gaussian Observational Data Scoring with Tiered
Background Knowledge. This class represents a score for causal discovery
using tiered background knowledge from observational Gaussian data; it
is used in the causal discovery function
[`tges()`](https://bjarkehautop.github.io/causalDisco/reference/tges.md).

## Arguments

- data:

  A numeric matrix with \\n\\ rows and \\p\\ columns. Each row
  corresponds to one observational realization.

- order:

  A vector specifying the order each variable. Can be either a vector of
  integers or an vector of prefixes. If integers, such that the ith
  entry will detail the order of the ith variable in the dataset. Must
  start at 1 an increase with increments of 1. If prefixes, must be in
  order.

- lambda:

  Penalization constant (see details).

- intercept:

  Logical; indicates whether an intercept is allowed in the linear
  structural equations (i.e., whether a nonzero mean is allowed).

## Details

The class implements a score which scores all edges contradicting the
ordering (edge going from a later tier to an earlier) to minus
\\\infty\\. If the edges does not contradict, the score is equal to that
of the standard BIC. The class implements an \\\ell_0\\-penalized
Gaussian maximum likelihood estimator. The penalization is a constant
(specified by the argument `lambda` in the constructor) times the number
of parameters of the DAG model. By default, the constant \\\lambda\\ is
chosen as \\\log(n)/2\\, which corresponds to the BIC score.

## Extends

Class `GaussL0penObsScore` from pcalg, directly.

All reference classes extend and inherit methods from `envRefClass`.

## Constructor

    new(
     "TemporalBIC",
     data = matrix(1, 1, 1),
     order =  rep(1,ncol(data)),
     lambda = 0.5 * log(nrow(data)),
     intercept = TRUE,
     ...
    )

## See also

[`tges()`](https://bjarkehautop.github.io/causalDisco/reference/tges.md)

## Author

Tobias Ellegaard Larsen

## Examples

``` r
# Simulate Gaussian data
set.seed(1405)
n <- 500
child_x <- rnorm(n)
child_y <- 0.5 * child_x + rnorm(n)
child_z <- 2 * child_x + child_y + rnorm(n)

adult_x <- child_x + rnorm(n)
adult_z <- child_z + rnorm(n)
adult_w <- 2 * adult_z + rnorm(n)
adult_y <- 2 * child_x + adult_w + rnorm(n)

simdata <- data.frame(
  child_x,
  child_y,
  child_z,
  adult_x,
  adult_z,
  adult_w,
  adult_y
)

# Define order in prefix way
kn <- knowledge(
  simdata,
  tier(
    child ~ tidyselect::starts_with("child"),
    adult ~ tidyselect::starts_with("adult")
  )
)

# Define TBIC score
t_score <- new("TemporalBIC", knowledge = kn, data = simdata)
# Run tges
tges_pre <- tges_run(t_score)

# Plot MPDAG
# plot(tges_pre)
```
