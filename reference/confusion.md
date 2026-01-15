# Compute confusion matrix for comparing two adjacency matrices

Two adjacency matrices are compared either in terms of adjacencies
(`type = "adj"`) or orientations (`type = "dir"`).

## Usage

``` r
confusion(est_amat, true_amat, type = "adj")
```

## Arguments

- est_amat:

  The estimated adjacency matrix, or `tpdag`/`cpdag` object as obtained
  from
  [tpc](https://bjarkehautop.github.io/causalDisco/reference/tpc.md) or
  [pc](https://bjarkehautop.github.io/causalDisco/reference/pc.md)

- true_amat:

  The true adjacency matrix, or `tpdag`/`cpdag` object as obtained from
  [tpc](https://bjarkehautop.github.io/causalDisco/reference/tpc.md) or
  [pc](https://bjarkehautop.github.io/causalDisco/reference/pc.md)

- type:

  String indicating whether the confusion matrix should be computed for
  adjacencies (`"adj"`, the default) or for (conditional) orientations
  (`dir`).

## Value

A list with entries `$tp` (number of true positives), `$tn` (number of
true negatives), `$fp` (number of false positives), and `$tp` (number of
false negatives).

## Details

Adjacency comparison: The confusion matrix is a cross-tabulation of
adjacencies. Hence, a true positive means that the two inputs agree on
the presence of an adjacency. A true negative means that the two inputs
agree on no adjacency. A false positive means that `est_amat` places an
adjacency where there should be none. A false negative means that
`est_amat` does not place an adjacency where there should have been one.

Orientation comparison: The orientation confusion matrix is conditional
on agreement on adjacency. This means that only adjacencies that are
shared in both input matrices are considered, and agreement wrt.
orientation is then computed only among these edges that occur in both
matrices. A true positive is a correctly placed arrowhead (1), a false
positive marks placement of arrowhead (1) where there should have been a
tail (0), a false negative marks placement of tail (0) where there
should have been an arrowhead (1), and a true negative marks correct
placement of a tail (0).

## Examples

``` r
x1 <- matrix(c(
  0, 0, 0, 0,
  1, 0, 1, 0,
  1, 0, 0, 0,
  0, 0, 1, 0
), 4, 4, byrow = TRUE)
x2 <- matrix(c(
  0, 0, 1, 0,
  1, 0, 0, 0,
  0, 0, 0, 0,
  1, 0, 1, 0
), 4, 4, byrow = TRUE)
nodes <- c("A", "B", "C", "D")
rownames(x1) <- colnames(x1) <- nodes
rownames(x2) <- colnames(x2) <- nodes

# confusion matrix for adjacencies
causalDisco:::confusion(x2, x1)
#> $tp
#> [1] 3
#> 
#> $tn
#> [1] 1
#> 
#> $fp
#> [1] 1
#> 
#> $fn
#> [1] 1
#> 

# confusion matrix for conditional orientations
causalDisco:::confusion(x2, x1, type = "dir")
#> $tp
#> [1] 2
#> 
#> $tn
#> [1] 2
#> 
#> $fp
#> [1] 1
#> 
#> $fn
#> [1] 1
#> 
```
