# Extract adjacency matrix from tpdag, cpdag, tpag or pag object

If the input is a tpdag or cpdag, the resulting adjacency matrix A is
"to-from" matrix encoded as follows:

- A(i,j) = 1 and A(j,i) = 0 means there is an edge j -\> i.

- A(j,i) = 1 and A(i,j) = 0 means there is an edge i -\> j.

- A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i
  and j, i - j.

- A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j.

## Usage

``` r
amat(x)
```

## Arguments

- x:

  `tpdag`, `cpdag`, `tpag`, `pag`, or `caugi` object as obtained from
  [tpc](https://bjarkehautop.github.io/causalDisco/reference/tpc.md),
  [pc](https://bjarkehautop.github.io/causalDisco/reference/pc.md),
  [tfci](https://bjarkehautop.github.io/causalDisco/reference/tfci.md),
  or [fci](https://bjarkehautop.github.io/causalDisco/reference/fci.md),
  respectively.

## Details

If the inout is a tpag or pag, there are four possible entry values: 0
(no edge), 1 (circle), 2 (arrowhead), 3 (tail). It is still encoded as a
"to-from" adjacency matrix, which means that e.g. A(i,j) = 1 places a
circle in the directed from j to i. For example, if A(i,j) = 1 and
A(j,i) = 2, we have that i o-\> j. Similarly, A(i,j) = 2 and A(j,i) = 3
mean that i \<- j.
