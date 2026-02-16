# F1 score

Computes F1 score from two `caugi` objects. It converts the `caugi`
objects to adjacency matrices and computes F1 score as \\2 \cdot TP/(2
\cdot TP + FP + FN)\\, where `TP` are truth positives, `FP` are false
positives, and `FN` are false negatives. If `TP + FP + FN = 0`, `1` is
returned. Only supports `caugi` objects with these edge types present
`-->`, `<-->`, `---` and no edge.

## Usage

``` r
f1_score(truth, est, type = c("adj", "dir"))
```

## Arguments

- truth:

  A `caugi` object representing the truth graph.

- est:

  A `caugi` object representing the estimated graph.

- type:

  Character string specifying the comparison type:

  - `"adj"`: adjacency comparison.

  - `"dir"`: orientation comparison conditional on shared adjacencies.

## Value

A numeric in \[0,1\].

## See also

Other metrics:
[`confusion()`](https://disco-coders.github.io/causalDisco/reference/confusion.md),
[`evaluate()`](https://disco-coders.github.io/causalDisco/reference/evaluate.md),
[`false_omission_rate()`](https://disco-coders.github.io/causalDisco/reference/false_omission_rate.md),
[`fdr()`](https://disco-coders.github.io/causalDisco/reference/fdr.md),
[`g1_score()`](https://disco-coders.github.io/causalDisco/reference/g1_score.md),
[`npv()`](https://disco-coders.github.io/causalDisco/reference/npv.md),
[`precision()`](https://disco-coders.github.io/causalDisco/reference/precision.md),
[`recall()`](https://disco-coders.github.io/causalDisco/reference/recall.md),
`reexports`,
[`specificity()`](https://disco-coders.github.io/causalDisco/reference/specificity.md)

## Examples

``` r
cg1 <- caugi::caugi(A %-->% B + C)
cg2 <- caugi::caugi(B %-->% A + C)
f1_score(cg1, cg2, type = "adj")
#> [1] 0.5
f1_score(cg1, cg2, type = "dir")
#> [1] 0
```
