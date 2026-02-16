# Plot Method for causalDisco Objects

This is the generic `plot()` function for objects of class `knowledge`
or `knowledgeable_caugi`. It dispatches to the class-specific plotting
methods
[`plot.knowledge()`](https://disco-coders.github.io/causalDisco/reference/plot.knowledge.md)
and
[`plot.knowledgeable_caugi()`](https://disco-coders.github.io/causalDisco/reference/plot.knowledgeable_caugi.md).

## Arguments

- x:

  An object to plot (class `knowledge` or `knowledgeable_caugi`).

- ...:

  Additional arguments passed to class-specific plot methods and to
  [`caugi::plot()`](https://caugi.org/reference/plot.html).

## Value

Invisibly returns the input object. The primary effect is the generated
plot.

## See also

[`plot.knowledge()`](https://disco-coders.github.io/causalDisco/reference/plot.knowledge.md),
[`plot.knowledgeable_caugi()`](https://disco-coders.github.io/causalDisco/reference/plot.knowledgeable_caugi.md),
[`caugi::plot()`](https://caugi.org/reference/plot.html)

## Examples

``` r
data(tpc_example)
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)
plot(kn)


cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
plot(disco_cd_tges)

```
