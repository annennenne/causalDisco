# Plot Method for causalDisco Objects

This is the generic `plot()` function for objects of class
[knowledge](https://disco-coders.github.io/causalDisco/reference/knowledge.md)
or
[disco](https://disco-coders.github.io/causalDisco/reference/disco.md).
It dispatches to the class-specific plotting methods
[`plot.Knowledge()`](https://disco-coders.github.io/causalDisco/reference/plot.Knowledge.md)
and
[`plot.Disco()`](https://disco-coders.github.io/causalDisco/reference/plot.Disco.md).

## Arguments

- x:

  An object to plot (class
  [knowledge](https://disco-coders.github.io/causalDisco/reference/knowledge.md)
  or
  [disco](https://disco-coders.github.io/causalDisco/reference/disco.md)).

- ...:

  Additional arguments passed to class-specific plot methods and to
  [`caugi::plot()`](https://caugi.org/reference/plot.html).

## Value

Invisibly returns the input object. The primary effect is the generated
plot.

## See also

[`plot.Knowledge()`](https://disco-coders.github.io/causalDisco/reference/plot.Knowledge.md),
[`plot.Disco()`](https://disco-coders.github.io/causalDisco/reference/plot.Disco.md),
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
