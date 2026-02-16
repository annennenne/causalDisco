# Generate Synthetic Data from a Linear Gaussian DAG

Generates synthetic data from a directed acyclic graph (DAG) specified
as a `caugi` graph object. Each node is modeled as a linear combination
of its parents plus additive Gaussian noise. Coefficients are randomly
signed with a minimum absolute value, and noise standard deviations are
sampled log-uniformly from a specified range. Custom node equations can
override automatic linear generation.

## Usage

``` r
generate_dag_data(
  cg,
  n,
  ...,
  standardize = TRUE,
  coef_range = c(0.1, 0.9),
  error_sd = c(0.3, 2),
  seed = NULL
)
```

## Arguments

- cg:

  A `caugi` graph object representing a DAG.

- n:

  Integer. Number of observations to simulate.

- ...:

  Optional named node equations to override automatic linear generation.
  Each should be an expression referencing all parent nodes.

- standardize:

  Logical. If `TRUE`, each column of the output is standardized to mean
  0 and standard deviation 1.

- coef_range:

  Numeric vector of length 2. Specifies the minimum and maximum absolute
  value of edge coefficients. Coefficients are randomly assigned a
  positive or negative sign. Must satisfy `coef_range[1] > 0` and
  `coef_range[2] >= coef_range[1]`.

- error_sd:

  Numeric vector of length 2. Specifies the range of standard deviations
  for the additive Gaussian noise at each node. A separate SD is sampled
  for each node from a log-uniform distribution. Must satisfy
  `error_sd[1] > 0` and `error_sd[2] >= error_sd[1]`.

- seed:

  Optional integer. Sets the random seed for reproducibility.

## Value

A `tibble` of simulated data with one column per node in the DAG,
ordered according to the graph's node order. Standardization is applied
if `standardize = TRUE`.

The returned tibble has an attribute `generating_model`, which is a list
containing:

- `sd`: Named numeric vector of node-specific noise standard deviations.

- `coef`: Named list of numeric vectors, where each element corresponds
  to a child node. For a child node, the vector stores the coefficients
  of its parent nodes in the linear structural equation. That is:
  `generating_model$coef[[child]][parent]` gives the coefficient of
  `parent` in the equation for `child`.

## Examples

``` r
cg <- caugi::caugi(A %-->% B, B %-->% C, A %-->% C, class = "DAG")

# Simulate 1000 observations
sim_data <- generate_dag_data(
  cg,
  n = 1000,
  coef_range = c(0.2, 0.8),
  error_sd = c(0.5, 1.5)
)

head(sim_data)
#> # A tibble: 6 × 3
#>        A      B      C
#>    <dbl>  <dbl>  <dbl>
#> 1 -0.311  1.49   1.66 
#> 2 -0.530  0.186  0.105
#> 3  0.681 -1.12  -0.909
#> 4  0.543  0.579  0.234
#> 5  1.04  -1.09  -1.09 
#> 6 -2.17   1.77   1.93 
attr(sim_data, "generating_model")
#> $dgp
#> $dgp$A
#> rnorm(n, sd = 1.433)
#> 
#> $dgp$B
#> A * -0.643 + rnorm(n, sd = 0.972)
#> 
#> $dgp$C
#> A * -0.673 + B * 0.535 + rnorm(n, sd = 0.67)
#> 
#> 

# Simulate with custom equation for node C
sim_data_custom <- generate_dag_data(
  cg,
  n = 1000,
  C = A^2 + B + rnorm(n, sd = 0.7),
  seed = 1405
)
head(sim_data_custom)
#> # A tibble: 6 × 3
#>         A      B      C
#>     <dbl>  <dbl>  <dbl>
#> 1  0.301   0.148 -0.114
#> 2 -1.13    2.02   1.71 
#> 3  0.727  -0.719 -0.696
#> 4 -0.179   0.781  0.276
#> 5  0.0492  0.840  0.449
#> 6  0.179   0.232 -0.641
attr(sim_data_custom, "generating_model")
#> $dgp
#> $dgp$A
#> rnorm(n, sd = 0.95)
#> 
#> $dgp$B
#> A * -0.856 + rnorm(n, sd = 1.266)
#> 
#> $dgp$C
#> A^2 + B + rnorm(n, sd = 0.7)
#> 
#> 
```
