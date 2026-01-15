# Remove an edge from a knowledge object

Drop a single directed edge specified by `from` and `to`. Errors if the
edge does not exist.

## Usage

``` r
remove_edge(kn, from, to)
```

## Arguments

- kn:

  A `knowledge` object.

- from:

  The source node (unquoted or character).

- to:

  The target node (unquoted or character).

## Value

The updated `knowledge` object.

## See also

Other knowledge functions:
[`+.knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/plus-.knowledge.md),
[`add_exogenous()`](https://bjarkehautop.github.io/causalDisco/reference/add_exogenous.md),
[`add_tier()`](https://bjarkehautop.github.io/causalDisco/reference/add_tier.md),
[`add_to_tier()`](https://bjarkehautop.github.io/causalDisco/reference/add_to_tier.md),
[`add_vars()`](https://bjarkehautop.github.io/causalDisco/reference/add_vars.md),
[`as_bnlearn_knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/as_bnlearn_knowledge.md),
[`as_pcalg_constraints()`](https://bjarkehautop.github.io/causalDisco/reference/as_pcalg_constraints.md),
[`as_tetrad_knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/as_tetrad_knowledge.md),
[`deparse_knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/deparse_knowledge.md),
[`forbid_edge()`](https://bjarkehautop.github.io/causalDisco/reference/forbid_edge.md),
[`forbid_tier_violations()`](https://bjarkehautop.github.io/causalDisco/reference/forbid_tier_violations.md),
[`get_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/get_tiers.md),
[`knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/knowledge.md),
[`remove_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/remove_tiers.md),
[`remove_vars()`](https://bjarkehautop.github.io/causalDisco/reference/remove_vars.md),
[`reorder_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/reorder_tiers.md),
[`reposition_tier()`](https://bjarkehautop.github.io/causalDisco/reference/reposition_tier.md),
[`require_edge()`](https://bjarkehautop.github.io/causalDisco/reference/require_edge.md),
[`seq_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/seq_tiers.md),
[`unfreeze()`](https://bjarkehautop.github.io/causalDisco/reference/unfreeze.md)

## Examples

``` r
# remove variables and their incident edges
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
print(kn)
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
#> ── Edges ──
#> 
#>  ✔  child_x1 → youth_x3
#> 

kn <- remove_edge(kn, child_x1, youth_x3)
print(kn)
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

kn <- remove_vars(kn, starts_with("child_"))
print(kn)
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
#> 1 youth_x3  youth 
#> 2 youth_x4  youth 
#> 3 oldage_x5 oldage
#> 4 oldage_x6 oldage
#> 

kn <- remove_tiers(kn, "child")
print(kn)
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label 
#> 1 youth 
#> 2 oldage
#> 
#> ── Variables ──
#> 
#>   var       tier  
#> 1 youth_x3  youth 
#> 2 youth_x4  youth 
#> 3 oldage_x5 oldage
#> 4 oldage_x6 oldage
#> 
```
