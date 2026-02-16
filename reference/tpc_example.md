# Simulated Life-Course Data

A small simulated data example intended to showcase the TPC algorithm.
Note that the variable name prefixes defines which period they are
related to ("child", "youth" or "oldage").

## Usage

``` r
tpc_example
```

## Format

A data.frame with 200 rows and 6 variables.

- child_x1:

  Structural equation: \\X_1 := \epsilon_1\\ with \\\epsilon_1 \sim
  \mathrm{Unif}\\0,1\\\\

- child_x2:

  Structural equation: \\X_2 := 2 \cdot X_1 + \epsilon_2\\ with
  \\\epsilon_2 \sim N(0,1)\\

- youth_x3:

  Structural equation: \\X_3 := \epsilon_3\\ with \\\epsilon_3 \sim
  \mathrm{Unif}\\0, 1\\\\

- youth_x4:

  Structural equation: \\X_4 := X_2 + \epsilon_4\\ with \\\epsilon_4
  \sim N(0,1)\\

- oldage_x5:

  Structural equation: \\X_5 := X_3^2 + X_3 - 3 \cdot X_2 + \epsilon_5\\
  with \\\epsilon_5 \sim N(0,1)\\

- oldage_x6:

  Structural equation: \\X_6 := X_4^3 + X_4^2 + 2 \cdot X_5 +
  \epsilon_6\\ with \\\epsilon_6 \sim N(0,1)\\

## References

Petersen, AH; Osler, M and Ekstrøm, CT (2021): Data-Driven Model
Building for Life-Course Epidemiology, American Journal of Epidemiology.

## Examples

``` r
data(tpc_example)
head(tpc_example)
#>   child_x2   child_x1    youth_x4 youth_x3  oldage_x6  oldage_x5
#> 1        0 -0.7104066 -0.07355602        1  6.4984994  3.0740123
#> 2        0  0.2568837 -1.16865142        1  0.3254685  1.9726530
#> 3        0 -0.2466919 -0.63474826        1  4.1298927  1.9666697
#> 4        1  1.6524574  0.97115845        0 -7.9064009 -4.5160676
#> 5        0 -0.9516186  0.67069597        0  1.7089134  0.7903853
#> 6        1  1.9549723 -0.65054654        0 -6.9758928 -3.2107342
```
