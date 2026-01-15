# Negative predictive value

Computes negative predictive value recall from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
Negative predictive value is defined as TN/(TN + FN), where TN are true
negatives and FN are false negatives. If TP + FN = 0, 0 is returned.

## Usage

``` r
npv(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
