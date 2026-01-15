# The Peter-Clark (PC) Algorithm for Causal Discovery

Run the PC algorithm for causal discovery using one of several engines.

## Usage

``` r
pc(engine = c("tetrad", "pcalg", "bnlearn"), test, alpha = 0.05, ...)
```

## Arguments

- engine:

  Character; which engine to use. Must be one of:

  `"tetrad"`

  :   Tetrad Java library.

  `"pcalg"`

  :   pcalg R package.

  `"bnlearn"`

  :   bnlearn R package.

- test:

  Character; name of the conditional‐independence test.

- alpha:

  Numeric; significance level for the CI tests.

- ...:

  Additional arguments passed to the chosen engine (e.g. test or
  algorithm parameters).

## Value

A function of class `"pc"` that takes a single argument `data` (a data
frame) and returns a `caugi` and a `knowledge` object.

## Details

For specific details on the supported scores, tests, and parameters for
each engine, see:

- [`TetradSearch`](https://bjarkehautop.github.io/causalDisco/reference/TetradSearch.md)
  for Tetrad,

- [`PcalgSearch`](https://bjarkehautop.github.io/causalDisco/reference/PcalgSearch.md)
  for pcalg,

- [`BnlearnSearch`](https://bjarkehautop.github.io/causalDisco/reference/BnlearnSearch.md)
  for bnlearn.

## Examples

``` r
data("tpc_example")

#### Using pcalg engine ####
# Recommended path using disco()
pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
disco(tpc_example, pc_pcalg)
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 

# or using pc_pcalg directly
pc_pcalg(tpc_example)
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 


#### Using bnlearn engine with required knowledge ####
kn <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth")
)


# Recommended path using disco()
pc_bnlearn <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
disco(tpc_example, pc_bnlearn, knowledge = kn)
#> Warning: vstructure youth_x4 -> oldage_x6 <- oldage_x5 is not applicable, because one or both arcs are oriented in the opposite direction.
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Variables ──
#> 
#>   var       tier 
#> 1 child_x1  NA   
#> 2 child_x2  NA   
#> 3 oldage_x5 NA   
#> 4 oldage_x6 NA   
#> 5 youth_x3  NA   
#> 6 youth_x4  NA   
#> 
#> ── Edges ──
#> 
#>  ✔  child_x1 → youth_x3
#>  ✔  child_x1 → youth_x4
#>  ✔  child_x2 → youth_x3
#>  ✔  child_x2 → youth_x4
#> 

# or using pc_bnlearn directly
pc_bnlearn <- pc_bnlearn |> set_knowledge(kn)
pc_bnlearn(tpc_example)
#> Warning: vstructure youth_x4 -> oldage_x6 <- oldage_x5 is not applicable, because one or both arcs are oriented in the opposite direction.
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 


#### Using tetrad engine with tier knowledge ####
# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  kn <- knowledge(
    tpc_example,
    tier(
      child ~ tidyselect::starts_with("child"),
      youth ~ tidyselect::starts_with("youth"),
      oldage ~ tidyselect::starts_with("oldage")
    )
  )

  # Recommended path using disco()
  pc_tetrad <- pc(engine = "tetrad", test = "fisher_z", alpha = 0.05)
  disco(tpc_example, pc_tetrad, knowledge = kn)

  # or using pc_tetrad directly
  pc_tetrad <- pc_tetrad |> set_knowledge(kn)
  pc_tetrad(tpc_example)
}
```
