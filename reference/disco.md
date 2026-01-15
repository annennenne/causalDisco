# Disco!!

Run a causal discovery method on a data frame.

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

  - [`pc()`](https://bjarkehautop.github.io/causalDisco/reference/pc.md) -
    PC algorithm,

  - [`fci()`](https://bjarkehautop.github.io/causalDisco/reference/fci.md) -
    FCI algorithm,

  - [`ges()`](https://bjarkehautop.github.io/causalDisco/reference/ges.md) -
    GES algorithm,

  - [`tges()`](https://bjarkehautop.github.io/causalDisco/reference/tges.md) -
    TGES algorithm,

  - [`tpc()`](https://bjarkehautop.github.io/causalDisco/reference/tpc.md) -
    TPC algorithm.

- knowledge:

  A `knowledge` object to be incorporated into the disco method. If
  `NULL` (default), the method is applied without additional knowledge.

## Value

A `caugi` and a `knowledge` (`knowledgeable_caugi`) object.

## Details

For specific details on the supported algorithms, scores, tests, and
parameters for each engine, see:

- [`TetradSearch`](https://bjarkehautop.github.io/causalDisco/reference/TetradSearch.md)
  for Tetrad,

- [`PcalgSearch`](https://bjarkehautop.github.io/causalDisco/reference/PcalgSearch.md)
  for pcalg,

- [`BnlearnSearch`](https://bjarkehautop.github.io/causalDisco/reference/BnlearnSearch.md)
  for bnlearn,

- [`CausalDiscoSearch`](https://bjarkehautop.github.io/causalDisco/reference/CausalDiscoSearch.md)
  for causalDisco.

## Examples

``` r
data("tpc_example")

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
