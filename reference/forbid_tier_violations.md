# Forbid all tier violations

Given a `knowledge` object with variables already assigned to tiers,
forbids every directed edge that runs from a higher-numbered tier down
into a lower-numbered tier.

## Usage

``` r
forbid_tier_violations(kn)
```

## Arguments

- kn:

  A `knowledge` object.

## Value

The same `knowledge` object with new forbidden edges added.

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
# automatically forbid edges that go from later tiers to earlier tiers
data(tpc_example)

kn <- knowledge(
  head(tpc_example),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# turn all tier violations to forbidden edges
kn2 <- forbid_tier_violations(kn)

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
print(kn2)
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
#> ── Edges ──
#> 
#>  ✖  oldage_x5 → child_x1
#>  ✖  oldage_x5 → child_x2
#>  ✖  oldage_x5 → youth_x3
#>  ✖  oldage_x5 → youth_x4
#>  ✖  oldage_x6 → child_x1
#>  ✖  oldage_x6 → child_x2
#>  ✖  oldage_x6 → youth_x3
#>  ✖  oldage_x6 → youth_x4
#>  ✖  youth_x3 → child_x1
#>  ✖  youth_x3 → child_x2
#>  ✖  youth_x4 → child_x1
#>  ✖  youth_x4 → child_x2
#> 
```
