
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

We provide a helper function to install Temurin JDK 25 on macOS and
Windows:

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
#> [1] "7.6.8"
#> 
#> $java_ok
#> [1] TRUE
#> 
#> $java_version
#> [1] "25.0.1"
#> 
#> $message
#> [1] "Tetrad found (version 7.6.8). Java version 25.0.1 is OK."
```

## Example

With causalDisco you can currently run causal discovery algorithms from
the package causalDisco itself, the Java library Tetrad, the R package
bnlearn, and the R package pcalg.

``` r
# options(causalDisco.tetrad.version = "7.6.9") # Memory issues are fixed on v7.6.9
library(causalDisco)
#> causalDisco startup:
#>   Java heap size requested: 2 GB
#>   Tetrad version: 7.6.8
#>   Java successfully initialized with 2 GB.
#>   To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
#>   Restart R to apply changes.

# load data
data("tpc_example")

# define background knowledge object
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# use Tetrad PC algorithm with conditional Gaussian test
# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  disco_tetrad_pc <- disco(data = tpc_example, method = tetrad_pc, knowledge = kn)

  # similarly, one could do
  tetrad_pc <- tetrad_pc |> set_knowledge(kn)
  disco_tetrad_pc_new <- tetrad_pc(tpc_example)
}

# use causalDisco's own tges algorithm with temporal BIC score
cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
```

You can visualize the resulting causal graph using the `plot()`
function:

``` r
plot(disco_cd_tges)
```

<img src="man/figures/README-plot-1.png" alt="A causal graph with the known tiers indicated by vertical positioning of the nodes." width="100%" /><img src="man/figures/README-plot-2.png" alt="A causal graph with the known tiers indicated by vertical positioning of the nodes." width="100%" />

## TODO

- Improve plot (use caugi)

  - For new features that are hard to do in grid (which caugi uses),
    maybe just insert the image in ggplot?

A rough WIP is here, which colors a rectangle around A and B:

``` r
# Make sure causalDisco is not loaded to avoid namespace conflicts with caugi
if ("package:causalDisco" %in% search()) {
  detach("package:causalDisco", unload = TRUE, character.only = TRUE)
}

library(caugi)
library(ggplot2)
library(ggplotify)
library(grid)

cg <- caugi(A %-->% B, C, D)
layout <- caugi_layout(cg)
print(layout)
layout$x <- c(0.5, 0.5, 0, 1)
layout$y <- c(0, 1, 0.5, 0.5)
print(layout)
plot_cg <- plot(cg, layout = layout)

# Wrap the grid plot as ggplot
gg <- as.ggplot(~grid.draw(plot_cg@grob))

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

![](man/figures/caugi-modified-plot.png)

- Implement a working `make_tikz` for these plots. See tikzDevice
  package, which can do it automatically from R plots:

``` r
library(caugi)
library(tikzDevice)
cg <- caugi(A %-->% B + C)
tikz("cg_plot.tex", width = 6, height = 4)
plot(
  cg,
  node_style = list(
    by_node = list(
      A = list(fill = "lightblue", col = "darkblue", lwd = 2),
      B = list(fill = "red")
    )
  )
)
dev.off()
```

If we want any changes we can modify the tikz code after generation.

- Make required work for our algorithms. It breaks when it internally
  calls `tpdag`, so look into that…

- Make score/test/alg names consistent. Currently a mix of snake_case,
  kebab-case, and period.case. - done?

- In documentation of defaults for tests maybe add the underlying engine
  defaults if they differ?

### Longterm

- Once more developed look at <https://github.com/py-why/causal-learn>
  and add it as an engine?

### Manging exported functions

- Various helper functions: `as_pcalg_constraints`, …,
  `is_knowledgeable_caugi`, …, `tetrad_graph`

### Bugfixes

- bnlearn has bug for old version of caugi. Fixed in PR \#149 in caugi.

- All of our algorithm does not work with required edges from knowledge
  objects (see e.g. [unit tests for
  tfci](https://github.com/BjarkeHautop/causalDisco/tree/master/tests/testthat/test-tfci.R)),
  `tpc`, `tges`, … Currently does nothing. Either make it work or throw
  error/warning if required edges are given.

  - Tried implementing it in the scores (e.g. `TemporalBdeu`) by giving
    it score -Inf if missing a required edge, but then it runs forever.
    I.e. adding the following to `local.score`

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

- v7.6.9 of Tetrad seems to fix the memory issues.

- Tetrad does not use required correctly in `fci` algorithm

``` r
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )
  
  tetrad_fci <- fci(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpc_example, method = tetrad_fci, knowledge = kn)
  edges <- output$caugi@edges
  edges
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

- Non-working Tetrad test/score arguments `"cci"` and
  `"mixed_variable_polynomial"`:

``` r
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  data("tpc_example")

  tetrad_pc <- pc(engine = "tetrad", test = "cci", alpha = 0.05)
  output <- disco(data = tpc_example, method = tetrad_pc)
}
#> Error in `.jcall()`:
#> ! java.lang.RuntimeException: Unrecognized basis type: 4
```

``` r
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  data("tpc_example")
  
  tetrad_ges <- ges(engine = "tetrad", score = "mixed_variable_polynomial")
  output <- disco(data = tpc_example, method = tetrad_ges)
}
#> Error in `.jcall()`:
#> ! java.lang.IllegalArgumentException: Variables list must not be null or empty.
```

### Documentation

- Make it clear in `?BnlearnSearch` (and similar for the others) that
  all algorithms aren’t currently fully supported.

- Use `snake_case` for all exported functions + arguments (even if the
  underlying engine uses e.g. camelCase).

- See how `mlr3` does it, and see their wiki on roxygen R6 guide
  [here](https://github.com/mlr-org/mlr3/wiki/Roxygen-R6-Guide).

- Make it clear somewhere what/how knowledge is supported in which
  algorithms. E.g. `pc` with engine pcalg only works with forbidden
  edges from knowledge objects and requires specifying both ways. It’s
  documented in `?as_pcalg_constaints` but should be more visible. In
  `?PcalgSearch` probably?

- List in documentation of `tfci`, … what kind of graph it returns.

- Make vignettes

### Standardization

- We are mixing between different things currently (since we rely on
  `caugi` are it uses `data.frame` and `S7`):
  - `tibble` vs `data.frame` (e.g. `knowledge` is `tibble` and
    `disco()$caugi@edges` is `data.frame`)
  - `S3` vs `S7`
  - More maybe?

### Adopt Tetrad v7.6.9

- Move to Tetrad v7.6.9. v7.6.9 removes the folder
  [algcomparison/algorithm/cluster](https://github.com/cmu-phil/tetrad/tree/v7.6.8/tetrad-lib/src/main/java/edu/cmu/tetrad/algcomparison/algorithm/cluster)

Was removed in this commit
<https://github.com/cmu-phil/tetrad/commit/295dceef6b83ac08ff0032fb194cf3ee5e429337#diff-adf829223cc59eac11682310f8a77c0ec3cf26a5b4310d75ec8edfaa86dd285b>

[Changelog](https://github.com/cmu-phil/tetrad/releases) item 14 says
“and a generalization of GFFC (Generalized Find Factor Clusters) of FOFC
and FTFC, providing multiple strategies for discovering latent
clusterings from measurement data.”

so we need to implement this in `causalDisco` (help?)

This also doesn’t work on 7.6.9:

``` r
> load_all()
ℹ Loading causalDisco
causalDisco startup:
  Java heap size requested: 2 GB
  Tetrad version: 7.6.9
  Java successfully initialized with 2 GB.
  To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
  Restart R to apply changes.
  
> var1 <- c(1.2, 2.3, 3.1, 4.5)
> var2 <- c(5.1, 6.2, 7.3, 8.4)
> df <- data.frame(var1, var2)
> tetrad_data <- rdata_to_tetrad(df)
Error in rdata_to_tetrad(df) : 
  java.lang.ClassNotFoundException: edu/cmu/tetrad/data/DiscreteVariable
```

KCI test from Tetrad in pc algorithm gives wrong graph in getting
started vignette (non-linear data) when version is 7.6.9? Works
correctly in 7.6.8. Fixed in unreleased version of Tetrad (see \#1946 in
Tetrad issues).

### CRAN TODO

- Add a copyright holder (`"cph"`) in persons field of DESCRIPTION
  (needed for CRAN, see
  [here](https://github.com/DavisVaughan/extrachecks))

- Update Description: field in DESCRIPTION to mention it wraps other
  packages, …

## Bugs & requests

Bug reports and feature requests are welcome:

👉 [open an issue](https://github.com/BjarkeHautop/causalDisco/issues).
