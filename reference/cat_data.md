# Simulated Categorical Data

A dataset created by discretizing the continuous `num_data` into 5
categorical levels per variable.

## Usage

``` r
cat_data
```

## Format

A data.frame with 1000 rows and 5 variables.

- X1:

  Categorical version of `num_data$X1`, with 5 levels a–e.

- X2:

  Categorical version of `num_data$X2`, with 5 levels a–e.

- X3:

  Categorical version of `num_data$X3`, with 5 levels a–e.

- Z:

  Categorical version of `num_data$Z`, with 5 levels a–e.

- Y:

  Categorical version of `num_data$Y`, with 5 levels a–e.

## Details

The R code used to generate this dataset is as follows:

    data(num_data)
    cat_data <- as.data.frame(
      lapply(num_data, function(x) cut(x, breaks = 5, labels = letters[1:5]))
    )

## See also

[num_data](https://disco-coders.github.io/causalDisco/reference/num_data.md)

## Examples

``` r
data(cat_data)
head(cat_data)
#>   X1 X2 X3 Z Y
#> 1  c  b  b c b
#> 2  d  b  a c c
#> 3  d  d  e b d
#> 4  e  b  b d d
#> 5  d  c  c c c
#> 6  d  c  c b d
```
