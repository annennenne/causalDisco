# Plot a Knowledge Object

Plot a knowledge object (tiers + required + forbidden)

## Usage

``` r
# S3 method for class 'knowledge'
plot(x, x_jitter = 0, vertex_size_scale = 1, ...)
```

## Arguments

- x:

  A `knowledge` object.

- x_jitter:

  Amount of jitter to apply to x positions of tiered nodes (default 0).

- vertex_size_scale:

  Scaling factor for vertex sizes (default 1).

- ...:

  Additional arguments passed to igraph `plot`.

## Value

A plot of the knowledge structure.

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

plot(kn)

```
