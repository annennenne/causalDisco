# Simulated Numerical Data

Simulated Numerical Data

## Usage

``` r
num_data
```

## Format

A data.frame with 1000 rows and 5 variables.

- X1:

  Structural equation: \\X_1 := \sqrt{Z} + \epsilon_1\\ with
  \\\epsilon_1 \sim \mathrm{Unif}\[0, 2\]\\

- X2:

  Structural equation: \\X_2 := 2 \cdot X_3 - \epsilon_2\\ with
  \\\epsilon_2 \sim N(5, 1)\\

- X3:

  Structural equation: \\X_3 := \epsilon_3\\ with \\\epsilon_3 \sim
  \mathrm{Unif}\[5, 10\]\\

- Z:

  Structural equation: \\Z := \|\epsilon_4\|\\ with \\\epsilon_4 \sim
  N(10, 1)\\

- Y:

  Structural equation: \\Y := X_1^2 + X_2 - X_3 - Z + \epsilon_5\\ with
  \\\epsilon_5 \sim N(10, 1)\\

## Details

The R code used to generate this dataset is as follows:

    set.seed(1405)
    n <- 1000
    Z <- abs(rnorm(n, mean = 10))
    X1 <- sqrt(Z) + runif(n, min = 0, max = 2)
    X3 <- runif(n, min = 5, max = 10)
    X2 <- 2 * X3 - rnorm(n, mean = 5)
    Y  <- X1^2 + X2 - X3 - Z + rnorm(n, mean = 10)
    num_data <- data.frame(X1, X2, X3, Z, Y)

## Examples

``` r
data(num_data)
head(num_data)
#>         X1        X2       X3         Z        Y
#> 1 3.900715  7.048325 6.964806 10.272479 15.35505
#> 2 4.736112  6.236746 5.666022 10.357262 23.36749
#> 3 4.657992 12.169805 9.127046  9.138338 25.32495
#> 4 5.176469  6.392344 6.101088 10.808335 26.75643
#> 5 4.535538 10.305236 7.465185 10.612735 22.67612
#> 6 4.885914 10.018856 7.413312  9.375931 28.03132
```
