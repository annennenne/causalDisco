# False Discovery Rate

Computes false discovery rate from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
False discovery rate is defined as FP/(FP + TP), where FP are false
positives and TP are true positives. If FP + TP = 0, 0 is returned.

## Usage

``` r
fdr(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
