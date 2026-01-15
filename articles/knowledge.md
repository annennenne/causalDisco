# Knowledge

``` r
library(causalDisco)
#> causalDisco startup:
#>   Java heap size requested: 2 GB
#>   Tetrad version: not installed
#>   Tetrad is not installed. Run install_tetrad() to install it.
#>   To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
#>   Restart R to apply changes.
```

This vignette demonstrates how to use the `knowledge` function to
incorporate prior knowledge into causal discovery algorithms. The
different supported knowledge types are explained below, along with
examples of how to create knowledge objects and use them with causal
discovery methods. All knowledge types can be freely combined.

At a conceptual level, all knowledge is represented as constraints on
edges, specifying which edges are allowed, disallowed, or required. Some
knowledge types provide higher-level abstractions for expressing common
modeling assumptions more conveniently.

## Required and forbidden knowledge

At the most basic level, prior knowledge is expressed as required or
forbidden edges between variables. These constraints apply to directed
edges in the causal graph.

- Required edges specify that a directed edge must exist between two
  variables.
- Forbidden edges specify that a directed edge is not allowed between
  two variables.

These constraints are specified using the `%-->%` (required) and
`%!-->%` (forbidden) operators, with the exclamation mark (`!`)
indicating negation of the edge, i.e. the absence of the edge.
Conceptually, this could be written as `%!(-->)%`, but we find this
syntax too verbose.

### Specifying required and forbidden edges

Suppose we want to require an edge from A to B, and forbid an edge from
B to C:

``` r
kn_1 <- knowledge(
  A %-->% B,
  B %!-->% C
)
```

This knowledge object can be visualized:

``` r
plot(kn_1)
```

![](knowledge_files/figure-html/plot%20required%20and%20forbidden%20knowledge-1.png)

The blue solid edge represents the required edge from A to B, while the
red dashed edge represents the forbidden edge from B to C.

If one wishes to remove some edges (either required or forbidden)
knowledge from an existing knowledge object, the `remove_edge` function
can be used. For example, to remove the required edge from A to B:

``` r
kn_1_removed <- remove_edge(kn_1, from = A, to = B)
plot(kn_1_removed)
```

![](knowledge_files/figure-html/remove%20required%20edge-1.png)

### Specifying required and forbidden edges in a dataset

We will use the `tpc_example` dataset from the causalDisco package for
the following examples:

``` r
data("tpc_example")
head(tpc_example)
#>   child_x2   child_x1    youth_x4 youth_x3  oldage_x6  oldage_x5
#> 1        0 -0.7104066 -0.07355602        1  6.4984994  3.0740123
#> 2        0  0.2568837 -1.16865142        1  0.3254685  1.9726530
#> 3        0 -0.2466919 -0.63474826        1  4.1298927  1.9666697
#> 4        1  1.6524574  0.97115845        0 -7.9064009 -4.5160676
#> 5        0 -0.9516186  0.67069597        0  1.7089134  0.7903853
#> 6        1  1.9549723 -0.65054654        0 -6.9758928 -3.2107342
```

We can pass the dataset to `knowledge`, which also checks that the
specified variables exist:

``` r
kn_2 <- knowledge(
  tpc_example,
  child_x1 %-->% youth_x3,
  child_x2 %!-->% oldage_x5
)
```

This knowledge object can also be visualized:

``` r
plot(kn_2)
```

![](knowledge_files/figure-html/plot%20required%20and%20forbidden%20knowledge%20with%20data-1.png)

The plot then plots all variables in the dataset, with the required
edges as blue solid edges and forbidden edges as red dashed edges.

#### Using tidyselect helpers

To make specifying variables easier, you can use tidyselect helpers such
as `starts_with`:

``` r
kn_3 <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth"),
  starts_with("oldage") %!-->% starts_with("youth")
)
```

This means, that all variables starting with “child” are required to
have edges to all variables starting with “youth”, and no variables
starting with “oldage” can have edges to any variables starting with
“youth”. We can visualize this:

``` r
plot(kn_3)
```

![](knowledge_files/figure-html/plot%20required%20and%20forbidden%20knowledge%20with%20tidyselect-1.png)

For a list of all available tidyselect helpers we refer to the
[tidyselect reference
documentation](https://tidyselect.r-lib.org/reference/index.html).

## Tiered knowledge

Tiered knowledge provides a higher-level abstraction for expressing
systematic ordering assumptions, such as temporal or logical precedence.
Internally, tiered knowledge is translated into a collection of
forbidden edges, but it is exposed separately because it provides a
concise and structured way to express common ordering assumptions.

For example, consider a dataset with three groups of variables: child,
youth, and old. We may wish to enforce that child variables precede
youth variables, which in turn precede old variables. This can be
expressed using tiered knowledge.

Tiered knowledge enforces that edges may only point from earlier tiers
to later tiers. Edges within the same tier are unrestricted unless
additional knowledge is supplied.

### Creating a tiered knowledge object

Suppose we observe variables over time: first the A’s, then the B’s, and
finally the C’s. This ordering implies that causal influences cannot go
backward in time (e.g., B’s cannot cause A’s). A tiered knowledge object
captures this temporal structure by specifying tiers and their
associated variables. If numeric tiers are used, lower numbers indicate
earlier tiers; otherwise, tiers are ordered by their appearance.

The following specifications encode the same tier structure:

``` r
kn <- knowledge(
  tier(
    1 ~ c(A1, A2),
    2 ~ c(B1, B2),
    3 ~ c(C1, C2)
  )
)

# Same object, since tiers are ordered numerically
kn_same <- knowledge(
  tier(
    1 ~ c(A1, A2),
    3 ~ c(C1, C2),
    2 ~ c(B1, B2)
  )
)

# Functionally equivalent, though not identical
kn_almost <- knowledge(
  tier(
    10 ~ c(A1, A2),
    30 ~ c(C1, C2),
    20 ~ c(B1, B2)
  )
)

# Again functionally equivalent
kn_also_almost <- knowledge(
  tier(
    A ~ c(A1, A2),
    B ~ c(B1, B2),
    C ~ c(C1, C2)
  )
)

# Has a letter, so tiers are ordered by appearance, thus functionally equivalent
kn_mixed <- knowledge(
  tier(
    3   ~ c(A1, A2),
    B   ~ c(B1, B2),
    1   ~ c(C1, C2)
  )
)
```

We can visualize the tiers using:

``` r
plot(kn)
```

![](knowledge_files/figure-html/plot%20tier%20knowledge-1.png)

The plot then shows the tiers as layers, with the earliest tiers to the
left and latest to the right.

Tidyselect helpers such as `starts_with` can also be used to define
tiers in a concise way, just as with required and forbidden edges.
Different tidyselect helpers can be freely combined within a tier
definition using `+`. For example, the following tiered knowledge object
defines two tiers, “young” and “old”, by combining tidyselect helpers:

``` r
kn_tier_tidyselect <- knowledge(
  tpc_example,
  tier(
    young ~ starts_with("child") + ends_with(c("3", "4")),
    old   ~ starts_with("old")
  )
)
```

## Exogenous variables knowledge

Exogenous variables are those that have no incoming edges in the causal
graph. That is, variables which are known causes but are not affected by
other variables. Exogenous variables can be specified using the
`exogenous` function within `knowledge`.

### Specifying exogenous variables

The most natural usage is to supply the dataset so that the variables
are checked for existence and selected correctly:

``` r
kn_exo_1 <- knowledge(
  tpc_example,
  exogenous("child_x1")
)
```

Instead of `exogenous`, you can also use the shorthand functions `exo`.

This knowledge object can be visualized:

``` r
plot(kn_exo_1)
```

![](knowledge_files/figure-html/plot%20exogenous%20knowledge-1.png)

Below we add both child_x1 and child_x2 as exogenous variables using
tidyselect helpers:

``` r
kn_exo_2 <- knowledge(
  tpc_example,
  exogenous(starts_with("child"))
)
plot(kn_exo_2)
```

![](knowledge_files/figure-html/exogenous%20knowledge%20with%20tidyselect-1.png)

## Combining different knowledge types

Different knowledge types can be freely combined in a single knowledge
object. For example, we can combine tiered knowledge with required and
forbidden edges:

``` r
kn_combined <- knowledge(
  tpc_example,
  tier(
    1 ~ starts_with("child"),
    2 ~ starts_with("youth"),
    3 ~ starts_with("oldage")
  ),
  child_x1 %-->% youth_x3,
  child_x1 %!-->% child_x2
)

plot(kn_combined)
```

![](knowledge_files/figure-html/combined%20knowledge-1.png)

## Using knowledge with causal discovery

Once prior knowledge has been specified, it can be supplied to causal
discovery algorithms. For example, we can use the temporal GES algorithm
tges with engine “causalDisco” and temporal BIC (“tbic”):

``` r
kn <- knowledge(
  tpc_example,
  tier(
    1 ~ starts_with("child"),
    2 ~ starts_with("youth"),
    3 ~ starts_with("oldage")
  )
)

cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
```

The causal discovery algorithms respects the provided knowledge. We can
plot the resulting causal graph:

``` r
plot(disco_cd_tges)
```

![](knowledge_files/figure-html/plot%20causal%20discovery%20with%20tier%20knowledge-1.png)![](knowledge_files/figure-html/plot%20causal%20discovery%20with%20tier%20knowledge-2.png)

The black edges are those inferred from the data.

## Engine specific information about knowledge

### bnlearn

All knowledge types are supported with bnlearn engine. Note, that you
can get a harmless(?) warning from bnlearn when using required
knowledge.

``` r
data("tpc_example")

kn <- knowledge(
  tpc_example,
  child_x1 %-->% youth_x3
)

bnlearn_pc <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
output <- disco(data = tpc_example, method = bnlearn_pc, knowledge = kn)
#> Warning in vstruct.apply(arcs = arcs, vs = vs, nodes = nodes, debug = debug):
#> vstructure child_x2 -> oldage_x5 <- youth_x3 is not applicable, because one or
#> both arcs are oriented in the opposite direction.
```

But the resulting causal graph respects the knowledge correctly
nevertheless.

``` r
plot(output)
```

![](knowledge_files/figure-html/plot%20bnlearn-1.png)![](knowledge_files/figure-html/plot%20bnlearn-2.png)

### causalDisco

WIP. Currently causalDisco only works correctly with tiered and
forbidden knowledge.

### pcalg

Only forbidden knowledge symmetric knowledge is supported for pcalg.
That is, edges that are forbidden in both directions. Thus, the only
type of knowledge that can be used with pcalg is knowledge created using
forbidden edges (`%!-->%`) without any directed knowledge or tiers, such
as this:

``` r
data("tpc_example")
kn <- knowledge(
  tpc_example,
  child_x1 %!-->% youth_x3,
  youth_x3 %!-->% child_x1
)
pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
output <- disco(data = tpc_example, method = pc_pcalg, knowledge = kn)
```

### Tetrad

WIP. All knowledge types are supported with Tetrad engine.
