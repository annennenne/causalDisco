# Generate a Bundle of Tier–Variable Formulas

Quickly create a series of two‐sided formulas for use with `tier()`,
where each formula maps a numeric tier index to a tidyselect
specification that contains the placeholder `i`. The placeholder `i` is
replaced by each element of `tiers` in turn, allowing you to write a
single template rather than many nearly identical formulas.

## Usage

``` r
seq_tiers(tiers, vars)
```

## Arguments

- tiers:

  An integer vector of tier indices (each \>= 1). These will appear as
  the left‐hand sides of the generated formulas.

- vars:

  A tidyselect specification (unevaluated) that *must* contain the
  special placeholder `i`, either as the symbol `i` or inside a string
  like `"…{i}…"`. For each value of `i` in `tiers`, that placeholder
  will be substituted and the resulting call used as the right‐hand side
  of a formula.

## Value

A list of two‐sided formulas, each of class `"tier_bundle"`. You can
pass this list directly to `tier()` (which will expand it
automatically).

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
[`remove_edge()`](https://bjarkehautop.github.io/causalDisco/reference/remove_edge.md),
[`remove_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/remove_tiers.md),
[`remove_vars()`](https://bjarkehautop.github.io/causalDisco/reference/remove_vars.md),
[`reorder_tiers()`](https://bjarkehautop.github.io/causalDisco/reference/reorder_tiers.md),
[`reposition_tier()`](https://bjarkehautop.github.io/causalDisco/reference/reposition_tier.md),
[`require_edge()`](https://bjarkehautop.github.io/causalDisco/reference/require_edge.md),
[`unfreeze()`](https://bjarkehautop.github.io/causalDisco/reference/unfreeze.md)

## Examples

``` r
# generate a bundle of tier formulas using a pattern with {i}
# here we create: 1 ~ matches("^child_x1$"), 2 ~ matches("^child_x2$")
data(tpc_example)

kn <- knowledge(
  tpc_example,
  tier(seq_tiers(1:2, matches("^child_x{i}$")))
)
print(kn)
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label
#> 1 1    
#> 2 2    
#> 
#> ── Variables ──
#> 
#>   var       tier 
#> 1 child_x1  1    
#> 2 child_x2  2    
#> 3 oldage_x5 NA   
#> 4 oldage_x6 NA   
#> 5 youth_x3  NA   
#> 6 youth_x4  NA   
#> 
```
