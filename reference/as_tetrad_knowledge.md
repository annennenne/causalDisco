# Convert to Tetrad `edu.cmu.tetrad.data.Knowledge`

Converts a `knowledge` object to a Tetrad
`edu.cmu.tetrad.data.Knowledge`. This requires `rJava`. This is used
internally, when setting knowledge with `set_knowledge` for methods
using the Tetrad engine. `set_knowledge` is used internally, when using
the `disco` function with knowledge given.

## Usage

``` r
as_tetrad_knowledge(kn)
```

## Arguments

- kn:

  A `knowledge` object.

## Value

A Java `edu.cmu.tetrad.data.Knowledge` object.

## See also

Other knowledge functions:
[`+.knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/plus-.knowledge.md),
[`add_exogenous()`](https://bjarkehautop.github.io/causalDisco/reference/add_exogenous.md),
[`add_tier()`](https://bjarkehautop.github.io/causalDisco/reference/add_tier.md),
[`add_to_tier()`](https://bjarkehautop.github.io/causalDisco/reference/add_to_tier.md),
[`add_vars()`](https://bjarkehautop.github.io/causalDisco/reference/add_vars.md),
[`as_bnlearn_knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/as_bnlearn_knowledge.md),
[`as_pcalg_constraints()`](https://bjarkehautop.github.io/causalDisco/reference/as_pcalg_constraints.md),
[`deparse_knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/deparse_knowledge.md),
[`forbid_edge()`](https://bjarkehautop.github.io/causalDisco/reference/forbid_edge.md),
[`forbid_tier_violations()`](https://bjarkehautop.github.io/causalDisco/reference/forbid_tier_violations.md),
[`get_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/get_tiers.md),
[`knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/knowledge.md),
[`remove_edge()`](https://bjarkehautop.github.io/causalDisco/reference/remove_edge.md),
[`remove_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/remove_tiers.md),
[`remove_vars()`](https://bjarkehautop.github.io/causalDisco/reference/remove_vars.md),
[`reorder_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/reorder_tiers.md),
[`reposition_tier()`](https://bjarkehautop.github.io/causalDisco/reference/reposition_tier.md),
[`require_edge()`](https://bjarkehautop.github.io/causalDisco/reference/require_edge.md),
[`seq_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/seq_tiers.md),
[`unfreeze()`](https://bjarkehautop.github.io/causalDisco/reference/unfreeze.md)

## Examples

``` r
# convert to Tetrad Knowledge via rJava
data(tpc_example)

kn <- knowledge(
  head(tpc_example),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  ),
  child_x1 %-->% youth_x3
)

jk <- try(as_tetrad_knowledge(kn)) # will run only if rJava/JVM available
#> Error in find_tetrad_jar() : 
#>   Tetrad directory not found. Please install Tetrad or set the TETRAD_DIR environment variable or tetrad.dir option.
try(print(jk)) # prints a Java reference if successful
#> [1] "Error in find_tetrad_jar() : \n  Tetrad directory not found. Please install Tetrad or set the TETRAD_DIR environment variable or tetrad.dir option.\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <simpleError in find_tetrad_jar(): Tetrad directory not found. Please install Tetrad or set the TETRAD_DIR environment variable or tetrad.dir option.>
```
