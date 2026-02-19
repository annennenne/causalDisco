# Convert Knowledge to pcalg Knowledge

pcalg only supports *undirected* (symmetric) background constraints:

- **fixed_gaps** - forbidding edges (zeros enforced)

- **fixed_edges** - requiring edges (ones enforced)

## Usage

``` r
as_pcalg_constraints(kn, labels = kn$vars$var, directed_as_undirected = FALSE)
```

## Arguments

- kn:

  A `knowledge` object. Must have no tier information.

- labels:

  Character vector of all variable names, in the exact order of your
  data columns. Every variable referenced by an edge in `kn` must appear
  here.

- directed_as_undirected:

  Logical (default `FALSE`). If `FALSE`, we require that every edge in
  `kn` has its mirror-image present as well, and will error if any are
  missing. If `TRUE`, we automatically mirror every directed edge into
  an undirected constraint.

## Value

A list with two elements, each an `n × n` logical matrix corresponding
to pcalg `fixed_gaps` and `fixed_edges` arguments.

## Details

This function takes a `knowledge` object (with only forbidden/required
edges, no tiers) and returns the two logical matrices in the exact
variable order you supply.

## Errors

- If the `Knowledge` object contains tiered knowledge.

- If `directed_as_undirected = FALSE` and any edge lacks its symmetrical
  counterpart. This can only hold for forbidden edges.

## See also

Other knowledge functions:
[`+.Knowledge()`](https://disco-coders.github.io/causalDisco/reference/plus-.Knowledge.md),
[`add_exogenous()`](https://disco-coders.github.io/causalDisco/reference/add_exogenous.md),
[`add_tier()`](https://disco-coders.github.io/causalDisco/reference/add_tier.md),
[`add_to_tier()`](https://disco-coders.github.io/causalDisco/reference/add_to_tier.md),
[`add_vars()`](https://disco-coders.github.io/causalDisco/reference/add_vars.md),
[`as_bnlearn_knowledge()`](https://disco-coders.github.io/causalDisco/reference/as_bnlearn_knowledge.md),
[`as_tetrad_knowledge()`](https://disco-coders.github.io/causalDisco/reference/as_tetrad_knowledge.md),
[`convert_tiers_to_forbidden()`](https://disco-coders.github.io/causalDisco/reference/convert_tiers_to_forbidden.md),
[`deparse_knowledge()`](https://disco-coders.github.io/causalDisco/reference/deparse_knowledge.md),
[`forbid_edge()`](https://disco-coders.github.io/causalDisco/reference/forbid_edge.md),
[`get_tiers()`](https://disco-coders.github.io/causalDisco/reference/get_tiers.md),
[`knowledge()`](https://disco-coders.github.io/causalDisco/reference/knowledge.md),
[`knowledge_to_caugi()`](https://disco-coders.github.io/causalDisco/reference/knowledge_to_caugi.md),
[`remove_edge()`](https://disco-coders.github.io/causalDisco/reference/remove_edge.md),
[`remove_tiers()`](https://disco-coders.github.io/causalDisco/reference/remove_tiers.md),
[`remove_vars()`](https://disco-coders.github.io/causalDisco/reference/remove_vars.md),
[`reorder_tiers()`](https://disco-coders.github.io/causalDisco/reference/reorder_tiers.md),
[`reposition_tier()`](https://disco-coders.github.io/causalDisco/reference/reposition_tier.md),
[`require_edge()`](https://disco-coders.github.io/causalDisco/reference/require_edge.md),
[`seq_tiers()`](https://disco-coders.github.io/causalDisco/reference/seq_tiers.md),
[`unfreeze()`](https://disco-coders.github.io/causalDisco/reference/unfreeze.md)

## Examples

``` r
# pcalg supports undirected constraints; build a tierless knowledge and convert
data(tpc_example)

kn <- knowledge(
  tpc_example,
  child_x1 %!-->% youth_x3,
  youth_x3 %!-->% child_x1
)

pc_constraints <- as_pcalg_constraints(kn, directed_as_undirected = FALSE)
print(pc_constraints)
#> $fixed_gaps
#>           child_x1 child_x2 oldage_x5 oldage_x6 youth_x3 youth_x4
#> child_x1     FALSE    FALSE     FALSE     FALSE     TRUE    FALSE
#> child_x2     FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> oldage_x5    FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> oldage_x6    FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> youth_x3      TRUE    FALSE     FALSE     FALSE    FALSE    FALSE
#> youth_x4     FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> 
#> $fixed_edges
#>           child_x1 child_x2 oldage_x5 oldage_x6 youth_x3 youth_x4
#> child_x1     FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> child_x2     FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> oldage_x5    FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> oldage_x6    FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> youth_x3     FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> youth_x4     FALSE    FALSE     FALSE     FALSE    FALSE    FALSE
#> 

# error paths
# using tiers
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  ),
  child_x1 %-->% youth_x3
)

try(as_pcalg_constraints(kn), silent = TRUE) # fails due to tiers

# using directed knowledge
kn <- knowledge(
  tpc_example,
  child_x1 %!-->% youth_x3
)

try(as_pcalg_constraints(kn), silent = TRUE) # fails due to directed knowledge
```
