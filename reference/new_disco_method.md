# Add a New causalDisco Method

This function allows you to create a new causal discovery method that
can be used with the
[`disco()`](https://disco-coders.github.io/causalDisco/reference/disco.md)
function. You provide a builder function that constructs a runner
object, along with metadata about the algorithm, and it returns a
closure that can be called with a data frame to perform causal discovery
and return a [caugi::caugi](https://caugi.org/reference/caugi.html)
object.

## Usage

``` r
new_disco_method(builder, name, engine, graph_class)
```

## Arguments

- builder:

  A function returning a runner

- name:

  Algorithm name

- engine:

  Engine identifier

- graph_class:

  Output graph class

## Value

A function of class `"disco_method"` that takes a single argument `data`
(a data frame) and returns a
[caugi::caugi](https://caugi.org/reference/caugi.html) object.

## See also

Other Extending causalDisco:
[`distribute_engine_args()`](https://disco-coders.github.io/causalDisco/reference/distribute_engine_args.md),
[`register_tetrad_algorithm()`](https://disco-coders.github.io/causalDisco/reference/register_tetrad_algorithm.md),
[`reset_tetrad_alg_registry()`](https://disco-coders.github.io/causalDisco/reference/reset_tetrad_alg_registry.md)
