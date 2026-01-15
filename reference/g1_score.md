# G1 score

Computes G1 score from a confusion matrix, see
[confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md).
G1 score is F1 score with reversed roles of 0/1 classifications, see
Petersen et al. 2022. The G1 score is defined as \\2 \* TN/(2 \* TN +
FN + FP)\\, where TN are true negatives, FP are false positives, and FN
are false negatives. If TN + FN + FP = 0, 1 is returned.

## Usage

``` r
g1_score(confusion)
```

## Arguments

- confusion:

  Confusion matrix as obtained from
  [confusion](https://bjarkehautop.github.io/causalDisco/reference/confusion.md)

## Value

A numeric in \[0,1\].

## References

Petersen, Anne Helby, et al. "Causal discovery for observational
sciences using supervised machine learning." arXiv preprint
arXiv:2202.12813 (2022).
