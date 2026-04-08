# Test for Vanishing Partial Correlations

This function simply calls the
[`pcalg::gaussCItest()`](https://rdrr.io/pkg/pcalg/man/condIndFisherZ.html)
function from the pcalg package.

## Usage

``` r
cor_test(x, y, conditioning_set, suff_stat)
```

## Arguments

- x:

  Index of x variable.

- y:

  Index of y variable.

- conditioning_set:

  Index vector of conditioning variable(s), possibly `NULL`.

- suff_stat:

  Sufficient statistic; A list with two elements, "C" and "n",
  corresponding to the correlation matrix and number of observations.

## Value

A numeric, which is the p-value of the test.
