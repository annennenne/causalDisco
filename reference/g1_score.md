# G1 score

Computes G1 score from two
[caugi::caugi](https://caugi.org/reference/caugi.html) objects. It
converts the [caugi::caugi](https://caugi.org/reference/caugi.html)
objects to adjacency matrices and computes G1 score defined as \\2 \cdot
TN/(2 \cdot TN + FN + FP)\\, where `TN` are truth negatives, `FP` are
false positives, and FN are false negatives. If `TN + FN + FP = 0`, `1`
is returned. Only supports
[caugi::caugi](https://caugi.org/reference/caugi.html) objects with
these edge types present `-->`, `<-->`, `---` and no edge.

## Usage

``` r
g1_score(truth, est, type = c("adj", "dir"))
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

## References

Petersen, Anne Helby, et al. "Causal discovery for observational
sciences using supervised machine learning." arXiv preprint
arXiv:2202.12813 (2022).

## See also

Other metrics:
[`confusion()`](https://disco-coders.github.io/causalDisco/reference/confusion.md),
[`evaluate()`](https://disco-coders.github.io/causalDisco/reference/evaluate.md),
[`f1_score()`](https://disco-coders.github.io/causalDisco/reference/f1_score.md),
[`false_omission_rate()`](https://disco-coders.github.io/causalDisco/reference/false_omission_rate.md),
[`fdr()`](https://disco-coders.github.io/causalDisco/reference/fdr.md),
[`npv()`](https://disco-coders.github.io/causalDisco/reference/npv.md),
[`precision()`](https://disco-coders.github.io/causalDisco/reference/precision.md),
[`recall()`](https://disco-coders.github.io/causalDisco/reference/recall.md),
`reexports`,
[`specificity()`](https://disco-coders.github.io/causalDisco/reference/specificity.md)

## Examples

``` r
cg1 <- caugi::caugi(A %-->% B + C)
cg2 <- caugi::caugi(B %-->% A + C)
g1_score(cg1, cg2, type = "adj")
#> [1] 0
g1_score(cg1, cg2, type = "dir")
#> [1] 0
```
