# Simulate a random DAG

Simulates a random directed acyclic graph adjacency (DAG) matrix with
the provided edge sparsity. The edge sparsity is the percentage of edges
that are absent, relative to a fully connected DAG.

## Usage

``` r
sim_dag(p, sparsity = NULL, sparsity_lim = c(0, 0.8), permute = TRUE)
```

## Arguments

- p:

  The number of nodes.

- sparsity:

  A numeric in \[0,1\] giving the proportion of possible edges that
  should be removed. If `sparsity` is supplied, the value in
  `sparsity_lim` is ignored.

- sparsity_lim:

  A numeric vector of length 2 giving the interval from which a sparsity
  value is drawn when `sparsity = NULL`. Both values must lie in
  \[0,1\].

- permute:

  If `FALSE`, the adjacency matrix will include nodes in their causal
  ordering. This is avoided by setting `permute = TRUE`, in which case
  the node order is permuted randomly.

## Value

An adjacency matrix.

## Examples

``` r
# Simulate a DAG adjacency matrix with 5 nodes and sparsity 0.5
sim_dag(5, sparsity = 0.5)
#>    x1 x2 x3 x4 x5
#> x1  0  0  0  1  0
#> x2  0  0  0  1  0
#> x3  0  0  0  0  1
#> x4  0  0  0  0  0
#> x5  1  0  0  1  0
```
