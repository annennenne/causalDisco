# Restricted Markov Equivalence Class Estimation Using Temporal Greedy Equivalence Search

Perform causal discovery using the temporal greedy equivalence search
algorithm.

## Usage

``` r
tges_run(score, verbose = FALSE)
```

## Arguments

- score:

  tiered scoring object to be used. At the moment only scores supported
  are

  - [`TemporalBIC`](https://bjarkehautop.github.io/causalDisco/reference/TemporalBIC-class.md)
    and

  - [`TemporalBDeu`](https://bjarkehautop.github.io/causalDisco/reference/TemporalBDeu-class.md).

- verbose:

  indicates whether debug output should be printed.

## Value

A `caugi` and a `knowledge` (`knowledgeable_caugi`) object.

## Author

Tobias Ellegaard Larsen

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
