# Negative Predictive Value

Computes negative predictive value from two PDAG `caugi` objects. It
converts the `caugi` objects to adjacency matrices and computes negative
predictive value as `TN/(TN + FN)`, where `TN` are truth negatives and
`FN` are false negatives. If `TN + FN = 0`, `1` is returned. Only
supports `caugi` objects with these edge types present `-->`, `<-->`,
`---` and no edge.

## Usage

``` r
npv(truth, est, type = c("adj", "dir"))
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
[`f1_score()`](https://disco-coders.github.io/causalDisco/reference/f1_score.md),
[`false_omission_rate()`](https://disco-coders.github.io/causalDisco/reference/false_omission_rate.md),
[`fdr()`](https://disco-coders.github.io/causalDisco/reference/fdr.md),
[`g1_score()`](https://disco-coders.github.io/causalDisco/reference/g1_score.md),
[`precision()`](https://disco-coders.github.io/causalDisco/reference/precision.md),
[`recall()`](https://disco-coders.github.io/causalDisco/reference/recall.md),
`reexports`,
[`specificity()`](https://disco-coders.github.io/causalDisco/reference/specificity.md)

## Examples

``` r
cg1 <- caugi::caugi(A %-->% B + C)
cg2 <- caugi::caugi(B %-->% A + C)
npv(cg1, cg2, type = "adj")
#> [1] 0
npv(cg1, cg2, type = "dir")
#> [1] 0
```
