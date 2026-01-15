# Specificity

Computes specificity from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
Specificity is defined as TN/(TN + FP), where TN are true negatives and
FP are false positives. If TN + FP = 0, 0 is returned.

## Usage

``` r
specificity(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
