# Specificity

Computes specificity from two PDAG
[caugi::caugi](https://caugi.org/reference/caugi.html) objects. It
converts the [caugi::caugi](https://caugi.org/reference/caugi.html)
objects to adjacency matrices and computes specificity as
`TN/(TN + FP)`, where `TN` are truth negatives and `FP` are false
positives. If `TN + FP = 0`, `1` is returned. Only supports
[caugi::caugi](https://caugi.org/reference/caugi.html) objects with
these edge types present `-->`, `<-->`, `---` and no edge.

## Usage

``` r
specificity(truth, est, type = c("adj", "dir"))
```

## Arguments

- truth:

  A [caugi::caugi](https://caugi.org/reference/caugi.html) object
  representing the truth graph.

- est:

  A [caugi::caugi](https://caugi.org/reference/caugi.html) object
  representing the estimated graph.

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
[`npv()`](https://disco-coders.github.io/causalDisco/reference/npv.md),
[`precision()`](https://disco-coders.github.io/causalDisco/reference/precision.md),
[`recall()`](https://disco-coders.github.io/causalDisco/reference/recall.md),
`reexports`

## Examples

``` r
cg1 <- caugi::caugi(A %-->% B + C)
cg2 <- caugi::caugi(B %-->% A + C)
specificity(cg1, cg2, type = "adj")
#> [1] 0
specificity(cg1, cg2, type = "dir")
#> [1] 0
```
