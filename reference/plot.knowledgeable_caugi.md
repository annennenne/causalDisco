# Plot a Causal Graph from a `knowledgeable_caugi` Object

This function visualizes a causal graph stored within a `caugi` object.
It incorporates background knowledge to highlight required and forbidden
edges. The required edges are drawn in blue, while forbidden edges are
shown in red and are dashed. If tiered knowledge is provided, the nodes
are arranged according to their tiers; otherwise, a circular layout is
used.

## Usage

``` r
# S3 method for class 'knowledgeable_caugi'
plot(x, ...)
```

## Arguments

- x:

  A `caugi` object containing the causal graph and knowledge.

- ...:

  Additional arguments passed to igraph `plot` and `plot.knowledge`.

## Value

A plot of the causal graph.

## Examples

``` r
data("tpc_example")

kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)

plot(disco_cd_tges)


```
