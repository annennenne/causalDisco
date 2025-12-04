
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causalDisco <img src="man/figures/hex.png" width="80" height="80" align="right" alt="" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/BjarkeHautop/causalDisco/graph/badge.svg)](https://app.codecov.io/gh/BjarkeHautop/causalDisco)
[![R-CMD-check](https://github.com/BjarkeHautop/causalDisco/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BjarkeHautop/causalDisco/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/causalDisco)](https://CRAN.R-project.org/package=causalDisco)
<!-- badges: end -->

`causalDisco` provides a unified interface for causal discovery on
observational data. It wraps multiple causal discovery backends under a
common, consistent syntax.

## Motivation

Causal discovery methods exist in many ecosystems, for example in
`bnlearn`, `pcalg`, or `Tetrad`, but their APIs vary widely.

`causalDisco` unifies them under one clear grammar, making it easy to
compare results, switch algorithms, and focus on scientific questions
rather than package quirks.

Time to hit the disco 🪩

## Installation

### Install `causalDisco`

To install `causalDisco` ensure you first have installed Rust and
Java/JDK as described below.

Then you can install the development version of `causalDisco` from
GitHub using `pak`:

``` r
pak::pkg_install("https://github.com/BjarkeHautop/causalDisco")
```

### Installing Rust

`causalDisco` depends on the package
[`caugi`](https://github.com/frederikfabriciusbjerre/caugi), which
requires Rust to be installed on your system. See [this
guide](https://www.rust-lang.org/tools/install) for instructions on how
to install Rust.

### Installing Java / JDK

`causalDisco` provides an interface to the Java library
[`Tetrad`](https://github.com/cmu-phil/tetrad) for causal discovery
algorithms. To use algorithms from `Tetrad` you need to install a Java
Development Kit (JDK). We recommend Eclipse Temurin (OpenJDK), available
at <https://adoptium.net>

You may also refer to `Tetrad`’s [Java setup
guide](https://github.com/cmu-phil/tetrad/wiki/Setting-up-Java-for-Tetrad).

The current supported version of `Tetrad` can then be installed by
calling

``` r
causalDisco::install_tetrad()
```

## Example

With `causalDisco` you can currently run causal discovery algorithms
from the packages `causalDisco` itself, the Java library `Tetrad`,
`bnlearn`, and `pcalg`.

``` r
library(causalDisco)
#> causalDisco startup:
#>   Java heap size requested: 2 GB
#>   Tetrad version: 7.6.8
#>   Java successfully initialized with 2 GB.
#>   To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
#>   Restart R to apply changes.

# load data
data("tpcExample")

# define background knowledge object
kn <- knowledge(
  tpcExample,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# use Tetrad PC algorithm with conditional Gaussian test
# Requires Tetrad to be installed
if (check_tetrad_install()$installed) {
  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  disco_tetrad_pc <- disco(data = tpcExample, method = tetrad_pc, knowledge = kn)

  # similarly, one could do
  tetrad_pc <- tetrad_pc |> set_knowledge(kn)
  disco_tetrad_pc_new <- tetrad_pc(tpcExample)
}

# use causalDisco's own tges algorithm with temporal BIC score
cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpcExample, method = cd_tges, knowledge = kn)
```

You can visualize the resulting causal graph using the `plot()`
function:

``` r
plot(disco_cd_tges)
```

<img src="man/figures/README-plot-1.png" alt="A causal graph with the known tiers indicated by vertical positioning of the nodes." width="100%" />

## Questions

- …

## TODO

### Clean up old files

- Remove old files such as `R/amat.R`, `R/compare.R`, `R/confusion.R`, …

### Dependencies

- Currently you can’t install `causalDisco` without having Java/JDK
  installed. Move all `rJava` stuff to optional and start up only
  initialize `rJava` if installed?

- Move from `tibble` to `data.table`? See also Standardization
  subsection.

- Remove old dependencies that are not used anymore (e.g. `MASS`)

### Bugfixes

#### causalDisco issues

- Some of our algorithms (`tfci`, more?) with engine `causalDisco` does
  not currently work correctly with tier knowledge

  - “tier” knowledge gives bidirectional edges that violate the tiers
    (see e.g. [unit tests for
    tfci](https://github.com/BjarkeHautop/causalDisco/tree/master/tests/testthat/test-tfci.R)):

``` r
data("tpcExample")
kn <- knowledge(
  tpcExample,
  tier(
    1 ~ starts_with("old"),
    2 ~ starts_with("youth"),
    3 ~ starts_with("child")
  )
)

my_tfci <- tfci(engine = "causalDisco", test = "fisher_z")
output <- disco(tpcExample, my_tfci, knowledge = kn)
edges <- output$caugi@edges

violations <- causalDisco:::check_tier_violations(edges, kn)
violations
#> # A tibble: 4 × 5
#>   from     edge  to        tier_from tier_to
#>   <chr>    <chr> <chr>         <int>   <int>
#> 1 child_x2 <->   oldage_x5         3       1
#> 2 youth_x3 <->   oldage_x5         2       1
#> 3 youth_x4 <->   oldage_x5         2       1
#> 4 youth_x4 <->   oldage_x6         2       1
```

Probably just the knowledge not knowing how to handle bi-directional
edges (since it works with e.g. `tges` on un-directed edges)?

- All of our algorithms (I think?) does not work with required edges
  from knowledge objects (see e.g. [unit tests for
  tfci](https://github.com/BjarkeHautop/causalDisco/tree/master/tests/testthat/test-tfci.R)),
  `tpc`, `tges`, … Currently does nothing.

- Piping as done above for `Tetrad` in the example section loses
  `$knowledge$tiers` information.

#### Tetrad issues

`Tetrad` v7.6.9 might fix some of these issues?

- `Tetrad` does not use tier knowledge correctly yet. If giving tier
  knowledge it still returns undirected edges between tiers (see [unit
  tests for
  pc](https://github.com/BjarkeHautop/causalDisco/tree/master/tests/testthat/test-pc.R#L1)).
  Could it be because the graph looks like this:

``` r
violations <- causalDisco:::check_tier_violations(edges, kn)
> violations
# A tibble: 2 × 5
  from      edge  to       tier_from tier_to
  <chr>     <chr> <chr>        <int>   <int>
1 oldage_x5 ---   youth_x3         3       2
2 oldage_x6 ---   youth_x4         3       2
```

And it only tries to fix it from the other way? I.e. if to and from were
swapped? More investigation needed …

It also breaks on required edges if paired with tier knowledge.

- Running `Tetrad` engine many times gives java.lang.RuntimeException.
  Try e.g. to run the code below.

``` r
data("tpcExample")
# This erros on my machine with error
# Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl,  : 
#  java.lang.RuntimeException


# Some memory leakage it seems like - is it our fault?

tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)

for (i in 1:1000) {
  kn <- knowledge(
    tpcExample,
    required(child_x1 ~ youth_x3)
  )
  output <- disco(data = tpcExample, method = tetrad_pc, knowledge = kn)
}
```

### Documentation

- Structure the documentation better. See See how `mlr3` does it, and
  see their wiki on roxygen R6 guide
  [here](https://github.com/mlr-org/mlr3/wiki/Roxygen-R6-Guide).

  - Make it clear somewhere what/how knowledge is supported in which
    algorithms. E.g. `pc` with engine `pcalg` only works with forbidden
    edges from knowledge objects and requires specifying both ways. It’s
    documented in `?as_pcalg_constaints` but should be more visible.

Maybe make it possible to check documentation of engine via `?pcalg` and
similar for the rest?

- List in documentation of `tfci`, … what kind of graph it returns.
  `tfci` gives a PAG, right? Currently it returns `graph_class: UNKNOWN`
  - Either wait/help for `caugi` to implement different classes
  - Or we implement a function that converts `caugi` output to specific
    graph classes based on algorithm/edges?
  - Update plotting function to work correctly with different graph
    classes.
- Make vignettes

### Standardization

- We are mixing between different things currently (since we rely on
  `caugi` are it uses `data.frame` and `S7`):
  - `tibble` vs `data.frame` (e.g. `knowledge` is `tibble` and
    `disco()$caugi@edges` is `data.frame`)
  - `S3` vs `S7`
  - More maybe?

### Adopt Tetrad v7.6.9

- Move to `Tetrad` v7.6.9. v7.6.9 removes the folder
  [algcomparison/algorithm/cluster](https://github.com/cmu-phil/tetrad/tree/v7.6.8/tetrad-lib/src/main/java/edu/cmu/tetrad/algcomparison/algorithm/cluster)

Was removed in this commit
<https://github.com/cmu-phil/tetrad/commit/295dceef6b83ac08ff0032fb194cf3ee5e429337#diff-adf829223cc59eac11682310f8a77c0ec3cf26a5b4310d75ec8edfaa86dd285b>

[Changelog](https://github.com/cmu-phil/tetrad/releases) item 14 says
“and a generalization of GFFC (Generalized Find Factor Clusters) of FOFC
and FTFC, providing multiple strategies for discovering latent
clusterings from measurement data.”

so we need to implement this in `causalDisco` (help?)

### CRAN TODO

- Add a copyright holder (`"cph"`) in persons field of DESCRIPTION
  (needed for CRAN, see
  [here](https://github.com/DavisVaughan/extrachecks))

- Update Description: field in DESCRIPTION to mention it wraps other
  packages, …

## Bugs & requests

Bug reports and feature requests are welcome:

👉 [open an issue](https://github.com/BjarkeHautop/causalDisco/issues).
