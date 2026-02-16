# Simulated Mixed Data

A dataset combining continuous and categorical variables. The first
three variables are replaced with categorical versions from `cat_data`.

## Usage

``` r
mix_data
```

## Format

A data.frame with 1000 rows and 5 variables.

- X1:

  Categorical, from `cat_data$X1`.

- X2:

  Categorical, from `cat_data$X2`.

- X3:

  Categorical, from `cat_data$X3`.

- Z:

  Numeric, same as `num_data$Z`.

- Y:

  Numeric, same as `num_data$Y`.

## Details

The R code used to generate this dataset is as follows:

    data(num_data)
    data(cat_data)
    mix_data <- num_data
    mix_data$X1 <- cat_data$X1
    mix_data$X2 <- cat_data$X2
    mix_data$X3 <- cat_data$X3

## See also

[num_data](https://disco-coders.github.io/causalDisco/reference/num_data.md),
[cat_data](https://disco-coders.github.io/causalDisco/reference/cat_data.md)

## Examples

``` r
data(mix_data)
head(mix_data)
#>   X1 X2 X3         Z        Y
#> 1  c  b  b 10.272479 15.35505
#> 2  d  b  a 10.357262 23.36749
#> 3  d  d  e  9.138338 25.32495
#> 4  e  b  b 10.808335 26.75643
#> 5  d  c  c 10.612735 22.67612
#> 6  d  c  c  9.375931 28.03132
```
