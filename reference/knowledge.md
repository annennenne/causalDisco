# Knowledge Mini-DSL Constructor

Constructs a `knowledge` object optionally initialized with a data frame
and extended with variable relationships expressed via formulas,
selectors, or infix operators:

    tier(1 ~ V1 + V2, exposure ~ E)
    V1 %-->% V3    # infix syntax for required edge from V1 to V3
    V2 %!-->% V3    # infix syntax for an edge from V2 to V3 that is forbidden
    exogenous(V1, V2)

## Usage

``` r
knowledge(...)
```

## Arguments

- ...:

  Arguments to define the knowledge object:

  - Optionally, a single data frame (first argument) whose column names
    initialize and freeze the variable set.

  - Zero or more mini-DSL calls: `tier()`,
    [`exogenous()`](https://caugi.org/reference/exogenous.html),
    `exo()`, or infix operators `%-->%`, `%!-->%`.

    - `tier()`: One or more two-sided formulas (`tier(1 ~ x + y)`), or a
      numeric vector.

    - [`exogenous()`](https://caugi.org/reference/exogenous.html) /
      `exo()`: Variable names or tidyselect selectors. Arguments are
      evaluated in order; only these calls are allowed.

## Value

A populated `knowledge` object.

## Details

Create a `knowledge` object using a concise mini-DSL with `tier()`,
[`exogenous()`](https://caugi.org/reference/exogenous.html) and infix
edge operators `%-->%` and `%!-->%`.

The first argument can be a data frame, which will be used to populate
the `knowledge` object with variable names. If you later add variables
with add\_\* verbs, this will throw a warning, since the knowledge
object will be *frozen*. You can unfreeze a knowledge object by using
the function `unfreeze(knowledge)`.

If no data frame is provided, the object is initially empty. Variables
can then be added via `tier()`, `forbidden()`, `required()`, infix
operators, or
[`add_vars()`](https://bjarkehautop.github.io/causalDisco/reference/add_vars.md).

- `tier()`: Assigns variables to tiers. Tiers may be numeric or string
  labels. The left-hand side (LHS) of the formula is the tier; the
  right-hand side (RHS) specifies variables. Variables can also be
  selected using tidyselect syntax: `tier(1 ~ starts_with("V"))`.

- `%-->%` and `%!-->%`: Infix operators to define required and forbidden
  edges, respectively. Both sides of the operator can use tidyselect
  syntax to select multiple variables.

- [`exogenous()`](https://caugi.org/reference/exogenous.html) / `exo()`:
  Mark variables as exogenous.

- Numeric vector shortcut for `tier()`: `tier(c(1, 2, 1))` assigns tiers
  by index to all existing variables.

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
data(tpc_example)

# knowledge objects are made with the knowledge() function
kn <- knowledge()


# knowledge objects contain tier information, forbidden and required edges
kn <- knowledge(
  tier(
    1 ~ V1 + V2,
    2 ~ V3
  ),
  V1 %-->% V2,
  V3 %!-->% V1
)

# if a data frame is provided, variable names are checked against it
kn <- knowledge(
  tpc_example,
  tier(
    1 ~ child_x1 + child_x2,
    2 ~ youth_x3 + youth_x4,
    3 ~ oldage_x5 + oldage_x6
  )
)

# throws error
try(
  knowledge(
    tpc_example,
    tier(
      1 ~ child_x1 + child_x2,
      2 ~ youth_x3 + youth_x4,
      3 ~ oldage_x5 + woops
    ) # wrong name
  )
)
#> 
#> ── Knowledge object ────────────────────────────────────────────────────────────
#> 
#> 
#> ── Tiers ──
#> 
#>   label
#> 1 1    
#> 2 2    
#> 3 3    
#> 
#> ── Variables ──
#> 
#>   var       tier 
#> 1 child_x1  1    
#> 2 child_x2  1    
#> 3 youth_x3  2    
#> 4 youth_x4  2    
#> 5 oldage_x5 3    
#> 6 oldage_x6 NA   
#> 

# using tidyselect helpers
kn <- knowledge(
  tpc_example,
  tier(
    1 ~ starts_with("child"), # can use tidyselect helpers
    2 ~ youth_x3 + youth_x4, # do not need quotes for tiers or variables
    3 ~ starts_with("oldage")
  ) # doesn't have to match data naming
)

# custom tier naming
kn <- knowledge(
  tpc_example,
  tier(
    "child" ~ starts_with("child"), # can use tidyselect helpers
    youth ~ starts_with("youth"), # do not need quotes for tiers
    elderly ~ starts_with("oldage")
  ) # doesn't have to match data naming
)

# There is also required and forbidden edges, which are specified like so
kn <- knowledge(
  tpc_example,
  child_x1 %-->% youth_x3,
  oldage_x6 %!-->% child_x1
)

# You can also add exogenous variables
kn <- knowledge(
  tpc_example,
  exogenous(child_x1),
  exo(child_x2) # shorthand
)

# You can also build knowledge with a verb pipeline
kn <-
  knowledge() |>
  add_vars(c("A", "B", "C", "D")) |> # knowledge now only takes these variables
  add_tier(One) |>
  add_to_tier("One" ~ A + B) |>
  add_tier(2, after = One) |>
  add_to_tier(2 ~ C + D) |>
  forbid_edge("A" ~ C) |>
  require_edge(A ~ B)

# Mix DSL start + verb refinement
kn <-
  knowledge(
    tier(1 ~ V5, 2 ~ V6),
    V5 %!-->% V6
  ) |>
  add_tier(3, after = "2") |>
  add_to_tier(3 ~ V7) |>
  add_exo(V2) |>
  add_exogenous(V3)

# Using seq_tiers for larger datasets
tpc_example <- as.data.frame(
  matrix(
    runif(100), # 100 random numbers in (0,1)
    nrow = 1,
    ncol = 100,
    byrow = TRUE
  )
)

names(tpc_example) <- paste0("X_", 1:100) # label the columns X_1,..., X_100

kn <- knowledge(
  tpc_example,
  tier(
    seq_tiers(
      1:100,
      ends_with("_{i}")
    )
  ),
  X_1 %-->% X_2
)

tpc_example <- data.frame(
  X_1 = 1,
  X_2 = 2,
  tier3_A = 3,
  Y5_ok = 4,
  check.names = FALSE
)

kn_seq_tiers2 <- knowledge(
  tpc_example,
  tier(
    seq_tiers(1:2, ends_with("_{i}")), # X_1, X_2
    seq_tiers(3, starts_with("tier{i}")), # tier3_
    seq_tiers(5, matches("Y{i}_ok")) # exact match
  )
)
```
