# Run the TPC Algorithm for Causal Discovery

Run a tier-aware variant of the PC algorithm that respects background
knowledge about a partial temporal order. Supply the temporal order via
a `knowledge` object. For backward compatibility, you may still pass
`order`, but that interface is deprecated and will be removed in a
future release.

## Usage

``` r
tpc_run(
  data = NULL,
  knowledge = NULL,
  alpha = 0.05,
  test = reg_test,
  suff_stat = NULL,
  method = "stable.fast",
  na_method = "none",
  orientation_method = "conservative",
  directed_as_undirected = FALSE,
  varnames = NULL,
  ...
)
```

## Arguments

- data:

  A data frame with the observed variables. Columns are variables. When
  using the deprecated `order` argument, variables should be prefixed
  with their period name (see examples). When using `knowledge`,
  prefixes are not required.

- knowledge:

  A `knowledge` object created with
  [`knowledge()`](https://disco-coders.github.io/causalDisco/reference/knowledge.md),
  encoding tier assignments and optional forbidden/required edges. This
  is the preferred way to provide temporal background knowledge.

- alpha:

  The alpha level used as the per-test significance threshold for
  conditional independence testing.

- test:

  A conditional independence test. The default `reg_test` uses a
  regression-based information-loss test. Another available option is
  `cor_test` which tests for vanishing partial correlations.
  User-supplied functions may also be used; see details for the required
  interface.

- suff_stat:

  A sufficient statistic. If supplied, it is passed directly to the test
  and no statistics are computed from `data`. Its structure depends on
  the chosen `test`.

- method:

  Skeleton construction method, one of `"stable"`, `"original"`, or
  `"stable.fast"` (default). See
  [`pcalg::skeleton()`](https://rdrr.io/pkg/pcalg/man/skeleton.html) for
  details.

- na_method:

  Handling of missing values, one of `"none"` (default; error on any
  `NA`), `"cc"` (complete-case analysis), or `"twd"` (test-wise
  deletion).

- orientation_method:

  Conflict-handling method when orienting edges. Currently only the
  conservative method is available.

- directed_as_undirected:

  Logical; if `TRUE`, treat any directed edges in `knowledge` as
  undirected during skeleton learning. This is due to the fact that
  pcalg does not allow directed edges in `fixedEdges` or `fixedGaps`.
  Default is `FALSE`.

- varnames:

  Character vector of variable names. Only needed when `data` is not
  supplied and all information is passed via `suff_stat`.

- ...:

  Additional arguments passed to
  [`pcalg::skeleton()`](https://rdrr.io/pkg/pcalg/man/skeleton.html)
  during skeleton construction.

## Value

A `caugi` and a `knowledge` (`knowledgeable_caugi`) object.

## Details

Any independence test implemented in pcalg may be used; see
[`pcalg::pc()`](https://rdrr.io/pkg/pcalg/man/pc.html). When
`na_method = "twd"`, test-wise deletion is performed: for `cor_test`,
each pairwise correlation uses complete cases; for `reg_test`, each
conditional test performs its own deletion. If you supply a user-defined
`test`, you must also provide `suff_stat`.

Temporal or tiered knowledge enters in two places:

- during skeleton estimation, candidate conditioning sets are pruned so
  they do not contain variables that are strictly after both endpoints;

- during orientation, any cross-tier edge is restricted to point forward
  in time.

The `order` argument is deprecated. If provided, it is converted to a
`knowledge` object by assigning variables to tiers using
[`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
for each prefix.

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
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  -->   child_x2 
#> 2 child_x2  -->   oldage_x5
#> 3 child_x2  -->   youth_x4 
#> 4 oldage_x5 ---   oldage_x6
#> 5 youth_x3  -->   oldage_x5
#> 6 youth_x4  -->   oldage_x6
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
#> ── Tiers ──
#> 
#>   tier 
#>   <chr>
#> 1 child
#> 2 youth
#> 3 old  
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  child
#> 2 child_x2  child
#> 3 youth_x3  youth
#> 4 youth_x4  youth
#> 5 oldage_x5 old  
#> 6 oldage_x6 old  

# or using my_tpc directly

my_tpc <- my_tpc |> set_knowledge(kn)
my_tpc(tpc_example)
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  -->   child_x2 
#> 2 child_x2  -->   oldage_x5
#> 3 child_x2  -->   youth_x4 
#> 4 oldage_x5 ---   oldage_x6
#> 5 youth_x3  -->   oldage_x5
#> 6 youth_x4  -->   oldage_x6
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
#> ── Tiers ──
#> 
#>   tier 
#>   <chr>
#> 1 child
#> 2 youth
#> 3 old  
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  child
#> 2 child_x2  child
#> 3 youth_x3  youth
#> 4 youth_x4  youth
#> 5 oldage_x5 old  
#> 6 oldage_x6 old  

# Using tpc_run() directly

tpc_run(tpc_example, knowledge = kn, alpha = 0.01)
#> ── caugi graph ─────────────────────────────────────────────────────────────────
#> Graph class: PDAG
#> 
#> ── Edges ──
#> 
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  -->   child_x2 
#> 2 child_x2  -->   oldage_x5
#> 3 child_x2  -->   youth_x4 
#> 4 oldage_x5 ---   oldage_x6
#> 5 youth_x3  -->   oldage_x5
#> 6 youth_x4  -->   oldage_x6
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
#> ── Tiers ──
#> 
#>   tier 
#>   <chr>
#> 1 child
#> 2 youth
#> 3 old  
#> ── Variables ──
#> 
#>   var       tier 
#>   <chr>     <chr>
#> 1 child_x1  child
#> 2 child_x2  child
#> 3 youth_x3  youth
#> 4 youth_x4  youth
#> 5 oldage_x5 old  
#> 6 oldage_x6 old  
```
