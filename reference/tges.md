# The Temporal GES algorithm for causal discovery

Run the Temporal GES algorithm for causal discovery using the
causalDisco engine.

## Usage

``` r
tges(engine = c("causalDisco"), score, ...)
```

## Arguments

- engine:

  Character; which engine to use. Must be one of:

  `"causalDisco"`

  :   causalDisco library.

- score:

  Character; name of the scoring function to use.

- ...:

  Additional arguments passed to the chosen engine (e.g. test or
  algorithm parameters).

## Value

A function of class `"tges"` that takes a single argument `data` (a data
frame) and returns a `caugi` and `knowledge` (`knowledgeable_caugi`)
object.

## Details

For specific details on the supported scores, see
[`CausalDiscoSearch`](https://bjarkehautop.github.io/causalDisco/reference/CausalDiscoSearch.md).
For additional parameters passed via `...`, see
[`tges_run()`](https://bjarkehautop.github.io/causalDisco/reference/tges_run.md).

## Examples

``` r
# Recommended route using disco:
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

my_tges <- tges(engine = "causalDisco", score = "tbic")

disco(tpc_example, my_tges, knowledge = kn)
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label
#> 1 child
#> 2 youth
#> 3 old  
#> 
#> ── Variables ──
#> 
#>   var       tier 
#> 1 child_x1  child
#> 2 child_x2  child
#> 3 youth_x3  youth
#> 4 youth_x4  youth
#> 5 oldage_x5 old  
#> 6 oldage_x6 old  
#> 

# another way to run it

my_tges <- my_tges |>
  set_knowledge(kn)
my_tges(tpc_example)
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 


# or you can run directly with tges_run()

data("tpc_example")

score_bic <- new(
  "TemporalBIC",
  data = tpc_example,
  nodes = colnames(tpc_example),
  knowledge = kn
)

res_bic <- tges_run(score_bic)
res_bic
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
```
