# Precision

Computes precision (aka positive predictive value) from a confusion
matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
Precision is defined as TP/(TP + FP), where TP are true positives and FP
are false positives. If TP + FP = 0, 0 is returned.

## Usage

``` r
precision(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
