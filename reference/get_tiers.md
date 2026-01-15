# Get tiers

Get tiers from a `knowledge` object.

## Usage

``` r
get_tiers(kn)
```

## Arguments

- kn:

  A `knowledge` object.

## Value

A tibble with the tiers.

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
kn <- knowledge(
  tier(
    1 ~ V1 + V2,
    2 ~ V3
  )
)

get_tiers(kn)
#> [1] "1" "2"
```
