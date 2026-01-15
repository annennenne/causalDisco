# False Omission Rate

Computes false omission rate from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
False omission rate is defined as FN/(FN + TN), where FN are false
negatives and TN are true negatives. If FN + TN = 0, 0 is returned.

## Usage

``` r
false_omission_rate(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
