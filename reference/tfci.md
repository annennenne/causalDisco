# The Temporal Fast Causal Inference (FCI) algorithm for causal discovery

Run the temporal FCI algorithm for causal discovery using causalDisco.

## Usage

``` r
tfci(engine = c("causalDisco"), test, alpha = 0.05, ...)
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

A function of class `"tfci"` that takes a single argument `data` (a data
frame) and returns a `caugi` and `knowledge` (`knowledgeable_caugi`)
object.

## Details

For specific details on the supported tests, see
[`CausalDiscoSearch`](https://bjarkehautop.github.io/causalDisco/reference/CausalDiscoSearch.md).
For additional parameters passed via `...`, see
[`tfci_run()`](https://bjarkehautop.github.io/causalDisco/reference/tfci_run.md).

## Examples

``` r
data("tpc_example")

kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

# Recommended path using disco()
my_tfci <- tfci(engine = "causalDisco", test = "fisher_z", alpha = 0.05)

disco(tpc_example, my_tfci, knowledge = kn)
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label 
#> 1 child 
#> 2 youth 
#> 3 oldage
#> 
#> ── Variables ──
#> 
#>   var       tier  
#> 1 child_x1  child 
#> 2 child_x2  child 
#> 3 youth_x3  youth 
#> 4 youth_x4  youth 
#> 5 oldage_x5 oldage
#> 6 oldage_x6 oldage
#> 

# or using my_tfci directly
my_tfci <- my_tfci |> set_knowledge(kn)
my_tfci(tpc_example)
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label 
#> 1 child 
#> 2 youth 
#> 3 oldage
#> 
#> ── Variables ──
#> 
#>   var       tier  
#> 1 child_x1  child 
#> 2 child_x2  child 
#> 3 youth_x3  youth 
#> 4 youth_x4  youth 
#> 5 oldage_x5 oldage
#> 6 oldage_x6 oldage
#> 

# Also possible: using tfci_run()
tfci_run(tpc_example, test = cor_test, knowledge = kn)
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label 
#> 1 child 
#> 2 youth 
#> 3 oldage
#> 
#> ── Variables ──
#> 
#>   var       tier  
#> 1 child_x1  child 
#> 2 child_x2  child 
#> 3 youth_x3  youth 
#> 4 youth_x4  youth 
#> 5 oldage_x5 oldage
#> 6 oldage_x6 oldage
#> 
```
