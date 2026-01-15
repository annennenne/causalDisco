# Recall

Computes recall from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
Recall is defined as TP/(TP + FN), where TP are true positives and FN
are false negatives. If TP + FN = 0, 0 is returned.

## Usage

``` r
recall(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
