# F1 score

Computes F1 score from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
The F1 score is defined as \\2 \* TP/(2 \* TP + FP + FN)\\, where TP are
true positives, FP are false positives, and FN are false negatives. If
TP + FP + FN = 0, 1 is returned.

## Usage

``` r
f1_score(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].
