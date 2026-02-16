# Perform Causal Discovery

Apply a causal discovery method to a data frame to infer causal
relationships on observational data. Supports multiple algorithms and
optionally incorporates prior knowledge.

## Usage

``` r
disco(data, method, knowledge = NULL)
```

## Arguments

- data:

  A data frame.

- method:

  A `disco_method` object representing a causal discovery algorithm.
  Available methods are

  - [`boss()`](https://disco-coders.github.io/causalDisco/reference/boss.md) -
    BOSS algorithm,

  - [`boss_fci()`](https://disco-coders.github.io/causalDisco/reference/boss_fci.md) -
    BOSS-FCI algorithm,

  - [`fci()`](https://disco-coders.github.io/causalDisco/reference/fci.md) -
    FCI algorithm,

  - [`gfci()`](https://disco-coders.github.io/causalDisco/reference/gfci.md) -
    GFCI algorithm,

  - [`ges()`](https://disco-coders.github.io/causalDisco/reference/ges.md) -
    GES algorithm,

  - [`grasp()`](https://disco-coders.github.io/causalDisco/reference/grasp.md) -
    GRaSP algorithm,

  - [`grasp_fci()`](https://disco-coders.github.io/causalDisco/reference/grasp_fci.md) -
    GRaSP-FCI algorithm,

  - [`gs()`](https://disco-coders.github.io/causalDisco/reference/gs.md) -
    GS algorithm,

  - [`iamb()`](https://disco-coders.github.io/causalDisco/reference/iamb-family.md),
    [`iamb_fdr()`](https://disco-coders.github.io/causalDisco/reference/iamb-family.md),
    [`fast_iamb()`](https://disco-coders.github.io/causalDisco/reference/iamb-family.md),
    [`inter_iamb()`](https://disco-coders.github.io/causalDisco/reference/iamb-family.md) -
    IAMB algorithms,

  - [`pc()`](https://disco-coders.github.io/causalDisco/reference/pc.md) -
    PC algorithm,

  - [`sp_fci()`](https://disco-coders.github.io/causalDisco/reference/sp_fci.md) -
    SP-FCI algorithm,

  - [`tfci()`](https://disco-coders.github.io/causalDisco/reference/tfci.md) -
    TFCI algorithm,

  - [`tges()`](https://disco-coders.github.io/causalDisco/reference/tges.md) -
    TGES algorithm,

  - [`tpc()`](https://disco-coders.github.io/causalDisco/reference/tpc.md) -
    TPC algorithm.

- knowledge:

  A `knowledge` object to be incorporated into the disco method. If
  `NULL` (default), the method is applied without additional knowledge.

## Value

A `caugi` and a `knowledge` (`knowledgeable_caugi`) object.

## Details

For specific details on the supported algorithms, scores, tests, and
parameters for each engine, see:

- [BnlearnSearch](https://disco-coders.github.io/causalDisco/reference/BnlearnSearch.md)
  for bnlearn,

- [CausalDiscoSearch](https://disco-coders.github.io/causalDisco/reference/CausalDiscoSearch.md)
  for causalDisco,

- [PcalgSearch](https://disco-coders.github.io/causalDisco/reference/PcalgSearch.md)
  for pcalg,

- [TetradSearch](https://disco-coders.github.io/causalDisco/reference/TetradSearch.md)
  for Tetrad.

## Examples

``` r
data(tpc_example)

# use pc with engine bnlearn and test fisher_z
my_pc <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.01)
pc_bnlearn <- disco(data = tpc_example, method = my_pc)
plot(pc_bnlearn)


# define tiered background knowledge
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# use gs with engine bnlearn and test cor and tiered background knowledge
my_pc_tiered <- pc(engine = "bnlearn", test = "cor", alpha = 0.01)
pc_tiered_bnlearn <- disco(
  data = tpc_example,
  method = my_pc_tiered,
  knowledge = kn
)
plot(pc_tiered_bnlearn)
```
