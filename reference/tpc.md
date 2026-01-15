# The Temporal Peter-Clark (PC) algorithm for causal discovery

Run the temporal PC algorithm for causal discovery using causalDisco.

## Usage

``` r
tpc(engine = c("causalDisco"), test, alpha = 0.05, ...)
```

## Arguments

- engine:

  Character; which engine to use. Must be one of:

  `"causalDisco"`

  :   causalDisco library.

- test:

  Character; name of the conditional‐independence test.

- alpha:

  Numeric; significance level for the CI tests.

- ...:

  Additional arguments passed to the chosen engine (e.g. test or
  algorithm parameters).

## Value

A function of class `"tpc"` that takes a single argument `data` (a data
frame) and returns a `caugi` and a `knowledge` (`knowledgeable_caugi`)
object.

## Details

For specific details on the supported tests, see
[`CausalDiscoSearch`](https://bjarkehautop.github.io/causalDisco/reference/CausalDiscoSearch.md).
For additional parameters passed via `...`, see
[`tpc_run()`](https://bjarkehautop.github.io/causalDisco/reference/tpc_run.md).

## Examples

``` r
# Load data
data(tpc_example)

# Build knowledge
kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    old ~ tidyselect::starts_with("old")
  )
)

# Recommended route using disco
my_tpc <- tpc(engine = "causalDisco", test = "fisher_z", alpha = 0.05)

disco(tpc_example, my_tpc, knowledge = kn)
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

# or using my_tpc directly

my_tpc <- my_tpc |> set_knowledge(kn)
my_tpc(tpc_example)
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

# Using tpc_run() directly

tpc_run(tpc_example, knowledge = kn, alpha = 0.01)
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
```
