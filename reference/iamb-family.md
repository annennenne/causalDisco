# IAMB Family of Causal Discovery Algorithms

Functions for causal discovery using variants of the Incremental
Association algorithm:

- `iamb`: Incremental Association (IAMB)

- `inter_iamb`: Interleaved Incremental Association (Inter-IAMB)

- `iamb_fdr`: Incremental Association with FDR (IAMB-FDR)

- `fast_iamb`: Fast Incremental Association (Fast-IAMB)

## Usage

``` r
iamb(engine = c("bnlearn"), test, alpha = 0.05, ...)

iamb_fdr(engine = c("bnlearn"), test, alpha = 0.05, ...)

fast_iamb(engine = c("bnlearn"), test, alpha = 0.05, ...)

inter_iamb(engine = c("bnlearn"), test, alpha = 0.05, ...)
```

## Arguments

- engine:

  Character; which engine to use. Must be one of:

  `"bnlearn"`

  :   bnlearn R package.

- test:

  Character; name of the conditional‐independence test.

- alpha:

  Numeric; significance level for the CI tests.

- ...:

  Additional arguments passed to the chosen engine (e.g., test or
  algorithm parameters).

## Value

Each function returns a `caugi` object (of class "PDAG") and a
`knowledge` object.

## Details

Each function supports the same engines and parameters. For details on
tests and parameters for each engine, see:

- [BnlearnSearch](https://disco-coders.github.io/causalDisco/reference/BnlearnSearch.md)
  for bnlearn.

## See also

Other causal discovery algorithms:
[`boss()`](https://disco-coders.github.io/causalDisco/reference/boss.md),
[`boss_fci()`](https://disco-coders.github.io/causalDisco/reference/boss_fci.md),
[`fci()`](https://disco-coders.github.io/causalDisco/reference/fci.md),
[`ges()`](https://disco-coders.github.io/causalDisco/reference/ges.md),
[`gfci()`](https://disco-coders.github.io/causalDisco/reference/gfci.md),
[`grasp()`](https://disco-coders.github.io/causalDisco/reference/grasp.md),
[`grasp_fci()`](https://disco-coders.github.io/causalDisco/reference/grasp_fci.md),
[`gs()`](https://disco-coders.github.io/causalDisco/reference/gs.md),
[`pc()`](https://disco-coders.github.io/causalDisco/reference/pc.md),
[`sp_fci()`](https://disco-coders.github.io/causalDisco/reference/sp_fci.md),
[`tfci()`](https://disco-coders.github.io/causalDisco/reference/tfci.md),
[`tges()`](https://disco-coders.github.io/causalDisco/reference/tges.md),
[`tpc()`](https://disco-coders.github.io/causalDisco/reference/tpc.md)

## Examples

``` r
data(tpc_example)

kn <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth")
)

##### iamb #####

# Recommended path using disco()
iamb_bnlearn <- iamb(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
disco(tpc_example, iamb_bnlearn, knowledge = kn)
#> 
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x1  -->   youth_x3 
#> 3 child_x1  -->   youth_x4 
#> 4 child_x2  -->   oldage_x5
#> 5 child_x2  -->   youth_x3 
#> 6 child_x2  -->   youth_x4 
#> 7 oldage_x5 -->   oldage_x6
#> 8 youth_x3  -->   oldage_x5
#> 9 youth_x4  -->   oldage_x6
#> ── Nodes ──
#> 
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  NA   
#> 2 child_x2  NA   
#> 3 oldage_x5 NA   
#> 4 oldage_x6 NA   
#> 5 youth_x3  NA   
#> 6 youth_x4  NA   
#> ── Edges ──
#> 
#>  ✔  child_x1 → youth_x3
#>  ✔  child_x1 → youth_x4
#>  ✔  child_x2 → youth_x3
#>  ✔  child_x2 → youth_x4

# or using iamb_bnlearn directly
iamb_bnlearn <- iamb_bnlearn |> set_knowledge(kn)
iamb_bnlearn(tpc_example)
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x1  -->   youth_x3 
#> 3 child_x1  -->   youth_x4 
#> 4 child_x2  -->   oldage_x5
#> 5 child_x2  -->   youth_x3 
#> 6 child_x2  -->   youth_x4 
#> 7 oldage_x5 -->   oldage_x6
#> 8 youth_x3  -->   oldage_x5
#> 9 youth_x4  -->   oldage_x6
#> ── Nodes ──
#> 
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
#> ── Knowledge object ────────────────────────────────────────────────────────────


# With all algorithm arguments specified
iamb_bnlearn <- iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05,
  max.sx = 2,
  debug = FALSE,
  undirected = TRUE
)

disco(tpc_example, iamb_bnlearn)
#> 
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x2  ---   oldage_x5
#> 3 oldage_x5 ---   oldage_x6
#> 4 oldage_x5 ---   youth_x3 
#> 5 oldage_x6 ---   youth_x4 
#> ── Nodes ──
#> 
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
#> ── Knowledge object ────────────────────────────────────────────────────────────

##### iamb_fdr #####

iamb_fdr_bnlearn <- iamb_fdr(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, iamb_fdr_bnlearn, knowledge = kn)
#> 
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x1  -->   youth_x3 
#> 3 child_x1  -->   youth_x4 
#> 4 child_x2  -->   oldage_x5
#> 5 child_x2  -->   youth_x3 
#> 6 child_x2  -->   youth_x4 
#> 7 oldage_x5 -->   oldage_x6
#> 8 youth_x3  -->   oldage_x5
#> 9 youth_x4  -->   oldage_x6
#> ── Nodes ──
#> 
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  NA   
#> 2 child_x2  NA   
#> 3 oldage_x5 NA   
#> 4 oldage_x6 NA   
#> 5 youth_x3  NA   
#> 6 youth_x4  NA   
#> ── Edges ──
#> 
#>  ✔  child_x1 → youth_x3
#>  ✔  child_x1 → youth_x4
#>  ✔  child_x2 → youth_x3
#>  ✔  child_x2 → youth_x4

##### fast_iamb #####

fast_iamb_bnlearn <- fast_iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, fast_iamb_bnlearn, knowledge = kn)
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x1  -->   youth_x3 
#> 3 child_x1  -->   youth_x4 
#> 4 child_x2  -->   oldage_x5
#> 5 child_x2  -->   youth_x3 
#> 6 child_x2  -->   youth_x4 
#> 7 oldage_x5 -->   oldage_x6
#> 8 youth_x3  -->   oldage_x5
#> 9 youth_x4  -->   oldage_x6
#> ── Nodes ──
#> 
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  NA   
#> 2 child_x2  NA   
#> 3 oldage_x5 NA   
#> 4 oldage_x6 NA   
#> 5 youth_x3  NA   
#> 6 youth_x4  NA   
#> ── Edges ──
#> 
#>  ✔  child_x1 → youth_x3
#>  ✔  child_x1 → youth_x4
#>  ✔  child_x2 → youth_x3
#>  ✔  child_x2 → youth_x4

#### inter_iamb #####

inter_iamb_bnlearn <- inter_iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, inter_iamb_bnlearn, knowledge = kn)
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x1  -->   youth_x3 
#> 3 child_x1  -->   youth_x4 
#> 4 child_x2  -->   oldage_x5
#> 5 child_x2  -->   youth_x3 
#> 6 child_x2  -->   youth_x4 
#> 7 oldage_x5 -->   oldage_x6
#> 8 youth_x3  -->   oldage_x5
#> 9 youth_x4  -->   oldage_x6
#> ── Nodes ──
#> 
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  NA   
#> 2 child_x2  NA   
#> 3 oldage_x5 NA   
#> 4 oldage_x6 NA   
#> 5 youth_x3  NA   
#> 6 youth_x4  NA   
#> ── Edges ──
#> 
#>  ✔  child_x1 → youth_x3
#>  ✔  child_x1 → youth_x4
#>  ✔  child_x2 → youth_x3
#>  ✔  child_x2 → youth_x4
```
