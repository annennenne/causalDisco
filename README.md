
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causalDisco <img src="man/figures/hex.png" width="80" height="80" align="right" alt="" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/BjarkeHautop/causalDisco/graph/badge.svg)](https://app.codecov.io/gh/BjarkeHautop/causalDisco)
[![R-CMD-check](https://github.com/BjarkeHautop/causalDisco/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BjarkeHautop/causalDisco/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/causalDisco)](https://CRAN.R-project.org/package=causalDisco)
<!-- badges: end -->

causalDisco provides a unified interface for causal discovery on
observational data. It wraps multiple causal discovery backends under a
common, consistent syntax.

## Motivation

Causal discovery methods exist in many ecosystems, for example in
bnlearn, pcalg, or Tetrad, but their APIs vary widely.

causalDisco unifies them under one clear grammar, making it easy to
compare results, switch algorithms, and focus on scientific questions
rather than package quirks.

Time to hit the disco 🪩

## Installation

### Install causalDisco

To install causalDisco ensure you first have installed Rust as described
below.

Then you can install the development version of causalDisco from GitHub
using pak:

``` r
pak::pkg_install("https://github.com/BjarkeHautop/causalDisco")
```

or with all suggested packages (note that this requires a valid Java /
JDK installation for rJava as described below):

``` r
pak::pkg_install("https://github.com/BjarkeHautop/causalDisco", dependencies = TRUE)
```

### Installing Rust

causalDisco depends on the package
[caugi](https://github.com/frederikfabriciusbjerre/caugi), which
requires Rust to be installed on your system. See
<https://www.rust-lang.org/tools/install> for instructions on how to
install Rust.

### Installing Java / JDK

causalDisco provides an interface to the Java library
[Tetrad](https://github.com/cmu-phil/tetrad) for causal discovery
algorithms. To use algorithms from Tetrad you need to install a Java
Development Kit (JDK) \>= 21. We recommend Eclipse Temurin (OpenJDK),
available at <https://adoptium.net> for all major operating systems.

Alternatively, we provide a helper function to install Temurin JDK 25 on
macOS and Windows:

``` r
causalDisco::install_java()
```

The current supported version of Tetrad can then be installed by calling

``` r
causalDisco::install_tetrad()
```

To verify everything is set up correctly you can run
`check_tetrad_install()`:

``` r
causalDisco::check_tetrad_install()
#> $installed
#> [1] TRUE
#> 
#> $version
#> [1] "7.6.10"
#> 
#> $java_ok
#> [1] TRUE
#> 
#> $java_version
#> [1] "25.0.1"
#> 
#> $message
#> [1] "Tetrad found (version 7.6.10). Java version 25.0.1 is OK."
```

## Example

With causalDisco you can currently run causal discovery algorithms from
the package causalDisco itself, the Java library Tetrad, the R package
bnlearn, and the R package pcalg.

``` r
library(causalDisco)
#> causalDisco startup:
#>   Java heap size requested: 2 GB
#>   Tetrad version: 7.6.10
#>   Java successfully initialized with 2 GB.
#>   To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
#>   Restart R to apply changes.

# load data
data(tpc_example)

# Define background knowledge object
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# Use Tetrad PC algorithm with conditional Gaussian test
# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  disco_tetrad_pc <- disco(data = tpc_example, method = tetrad_pc, knowledge = kn)

  # Similarly, one could do
  tetrad_pc <- tetrad_pc |> set_knowledge(kn)
  disco_tetrad_pc_new <- tetrad_pc(tpc_example)
}

# Use causalDisco's own tges algorithm with temporal BIC score
cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
```

You can visualize the resulting causal graph using the `plot()`
function:

``` r
plot(disco_cd_tges)
```

<img src="man/figures/README-plot-1.png" alt="A causal graph with the known tiers indicated by vertical positioning of the nodes." width="100%" />

## Questions

- Updated getting started, knowledge vignettes based on the feedback.
  Anything missing?

- Made a causal discovery vignette (essentially old content of getting
  started vignette, but examples modified). Anything missing?

- Made a visualization vignette showing knowledge plotting and TikZ
  export. Anything missing?

## TODO

- Register a custom edge type for caugi to represent forbidden edges
  differently than normal directed edges. Would simplify plotting logic
  and easier to extend later.

``` r
# Doesn't work atm because CRAN version requires length 3 (will work in next release)
caugi::register_caugi_edge(
  glyph = "!-->",
  tail_mark = "arrow",
  head_mark = "tail",
  class = "directed",
  symmetric = FALSE
)
```

- Don’t clash namespaces with caugi. We currently share `nodes()` and
  `edges()`. Make PR in caugi that makes their `nodes()` and `edges()`
  it a S3/S7 class? Merged in caugi. But if we want to submit to CRAN
  before they do we can’t use it for now…

A rough WIP is here, which colors a rectangle around A and B:

``` r
library(caugi)
#> 
#> Attaching package: 'caugi'
#> The following objects are masked from 'package:causalDisco':
#> 
#>     edges, nodes
library(ggplot2)
library(ggplotify)

cg <- caugi(A %-->% B, C, D)
layout <- caugi_layout(cg)
print(layout)
#>   name   x   y
#> 1    A 0.0 0.0
#> 2    B 0.0 1.0
#> 3    C 0.1 0.5
#> 4    D 0.2 0.5
layout$x <- c(0.5, 0.5, 0, 1)
layout$y <- c(0, 1, 0.5, 0.5)
print(layout)
#>   name   x   y
#> 1    A 0.5 0.0
#> 2    B 0.5 1.0
#> 3    C 0.0 0.5
#> 4    D 1.0 0.5
plot_cg <- plot(cg, layout = layout)

# Wrap the grid plot as ggplot
gg <- as.ggplot(plot_cg@grob)

# Add rectangle
gg +
  annotate(
    "rect",
    xmin = layout$x[1] - 0.05,
    xmax = layout$x[2] + 0.05,
    ymin = layout$y[1],
    ymax = layout$y[2],
    fill = "red",
    alpha = 0.3
  )
```

<img src="man/figures/README-plot wip-1.png" alt="" width="100%" />

- Make required work for our algorithms. It breaks when it internally
  calls `tpdag`, so look into that…

- In documentation of defaults for tests maybe add the underlying engine
  defaults if they differ?

- In tests (and examples) don’t use `tpc_example` all the time and
  instead also use other data sets (e.g. num_data, cat_data, mix_data).
  (this also avoids that we currently simulate data in tests when
  needing an e.g. numerical dataset). Done mostly I think? Just missing
  `cat_ord_data` usage iirc.

- Add all algs/scores/tests from the backends (start with Tetrad). Check
  if we currently document ones we haven’t implemented yet.

  - Missing scores: `"Instance-specific Augmented SEM BIC Score"`. Get
    this error when implementing:
    `Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl, : java.lang.NullPointerException: Cannot invoke "edu.cmu.tetrad.data.Knowledge.getTestingData()" because "this.knowledge" is null`
    (see branch `Add-isa-sem-bic-score-to-Tetrad`).

- Implement fci from pcalg

- Implement BOSS + grasp from Tetrad

- Update evaluation and confusion metrics (use caugi?)

### Bugfixes

- Setting `mc = TRUE` (or `mc_test = TRUE`) errors in Tetrad. Remove the
  argument for now (and maybe fix later).

- Setting `precompute_covariances = FALSE` errors in Tetrad. Remove the
  argument for now (and maybe fix later).

- Tried implementing required edges in the scores (e.g. `TemporalBdeu`)
  by giving it score -Inf if missing a required edge, but then it runs
  forever. I.e. adding the following to `local.score`

``` r
vertex_name <- colnames(pp.dat$data)[vertex]
req_parents <- kn$edges |>
dplyr::filter(status == "required", to == vertex_name) |>
dplyr::pull(from)

parent_names <- colnames(pp.dat$data)[parents]
missing_required <- !all(req_parents %in% parent_names)
if (missing_required) {
  return(-Inf)
}
```

The algorithm needs to be modified when having required edges, I think.

Should be easier to fix for test based algorithms? Just check if
required edges are present after skeleton phase and add them if missing?
(and forbid them from being removed in orientation phase). Look at
fixedEdges in pcalg.

- Look into how (if) possible to pass to pcalg.

- Piping as done above for Tetrad in the example section loses
  `$knowledge$tiers` information due to how builders/closures capture
  knowledge.

  - Fixing requires refactoring the disco_method builder design I think.

#### Tetrad issues

- Tetrad does not use required correctly in `fci` algorithm

``` r
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )
  
  tetrad_fci <- fci(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpc_example, method = tetrad_fci, knowledge = kn)
  edges(output$caugi)
}
#>         from   edge        to
#>       <char> <char>    <char>
#> 1:  child_x2    o-o  child_x1
#> 2:  child_x2    o-> oldage_x5
#> 3:  child_x2    o-o  youth_x4
#> 4: oldage_x5    --> oldage_x6
#> 5:  youth_x3    o-> oldage_x5
#> 6:  youth_x4    --> oldage_x6
```

Fixed in unreleased version of Tetrad (see \#1947 in Tetrad issues).

### Documentation

- Make it clear in `?BnlearnSearch` (and similar for the others) that
  all algorithms aren’t currently fully supported.

- List in documentation of `tfci`, … what kind of graph it returns.

- Figure out how to not repeat the documentation of e.g. penalty
  discount in TetradSearch R6 class.

### CRAN TODO

- Update Description: field in DESCRIPTION to mention it wraps other
  packages, …

## Bugs & requests

Bug reports and feature requests are welcome:

[open an issue](https://github.com/BjarkeHautop/causalDisco/issues).
