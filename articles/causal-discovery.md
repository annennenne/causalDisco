# Causal Discovery

``` r
library(causalDisco)
#> causalDisco startup:
#>   Java heap size requested: 2 GB
#>   Tetrad version: not installed
#>   Tetrad is not installed. Run install_tetrad() to install it.
#>   To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
#>   Restart R to apply changes.
```

This vignette provides an overview of the `causalDisco` package, which
offers tools for causal discovery from observational data. It covers the
main features of the package, including various causal discovery
algorithms, knowledge incorporation, and result visualization.

TODO: CHANGE EXAMPLE …

## Simple example of causal discovery

The example discussed in this section is inspired by the Julia package
[CausalDiscovery.jl](https://mschauer.github.io/CausalInference.jl/latest/)
with their PC algorithm example, which can be found here [PC algorithm
example in
CausalDiscovery.jl](https://mschauer.github.io/CausalInference.jl/latest/examples/pc_basic_examples/).

We will consider data from the following DAG, which is also discussed in
chapter 2 of Judea Pearl’s book.

![](dag.png)

We create data from a linear Gaussian model corresponding to the above
DAG:

``` r
set.seed(1405)
n <- 1000
x <- rnorm(n)
v <- x + rnorm(n) * 0.5
w <- x + rnorm(n) * 0.5
z <- v + w + rnorm(n) * 0.5
s <- z + rnorm(n) * 0.5

data_linear <- data.frame(x = x, v = v, w = w, z = z, s = s)
head(data_linear)
#>            x           v          w          z          s
#> 1  0.2724785  0.07687489  1.2131219  1.4542158  0.2649549
#> 2  0.3572619  0.81022383  0.4049109  1.5767672  1.5394264
#> 3 -0.8616620 -0.68388923 -0.3195188 -1.1415913 -1.2647045
#> 4  0.8083350  1.61458098  1.2132504  3.0666677  3.0334703
#> 5  0.6127352  0.42484707  0.4253022  0.7889983  0.7937421
#> 6 -0.6240686 -0.23225274 -0.7201852 -0.4876655 -0.7977492
```

We can use the PC algorithm from either the “tetrad”, “pcalg”, or
“bnlearn” engine to discover the causal structure. Below, we set up the
PC method with Fisher’s Z test and a significance level of 0.05 and
“pcalg” and “bnlearn” engines.

``` r
pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
pc_bnlearn <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)

pc_result_pcalg <- disco(data_linear, method = pc_pcalg)
pc_result_bnlearn <- disco(data_linear, method = pc_bnlearn)
```

We can visualize the results from each engine:

``` r
par(mfrow = c(1, 2))
plot(pc_result_pcalg, main = "PC (pcalg)")
plot(pc_result_bnlearn, main = "PC (bnlearn)")
```

![](causal-discovery_files/figure-html/plot%20pc%20results%20simple-1.png)

``` r
par(mfrow = c(1, 1))
```

(Note, that it doesn’t work correctly for bnlearn, since
[`as_caugi()`](https://caugi.org/reference/as_caugi.html) in caugi had a
bug. I fixed it in PR \#149, so it will work in the next release of
caugi.)

The first notable feature of this plot is that some edges have arrows,
while others do not. For instance, the edge from `v` to `z` is directed,
indicating that `v` influences `z`, but not vice versa. In contrast, the
edge between `x` and `w` has no arrows at either end, showing that the
direction of causal influence cannot be determined from the data alone.
Both directions; `x` to `w` and `w` to `x`, are consistent with the
observed data. We can demonstrate this by reversing the direction of
influence in the data-generating process above and applying the PC
algorithm to the new data set:

``` r
set.seed(1405)
n <- 1000
v <- rnorm(n)
x <- v + rnorm(n) * 0.5
w <- x + rnorm(n) * 0.5
z <- v + w + rnorm(n) * 0.5
s <- z + rnorm(n) * 0.5

data_linear_reversed <- data.frame(x = x, v = v, w = w, z = z, s = s)

pc_pcalg_reversed <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
pc_result_reversed <- disco(data_linear_reversed, method = pc_pcalg_reversed)
plot(pc_result_reversed, main = "PC (pcalg) reversed")
```

![](causal-discovery_files/figure-html/pc%20algorithm%20reversed-1.png)

We learn the same causal structure as before, demonstrating that the
direction of influence between `x` and `w` cannot be determined from the
data alone.

### Non-linear example

Here, we simulate data the same DAG structure as above, but with a
non-linear relationships between the variables.

``` r
set.seed(1405)
n <- 1000
x <- runif(n, min = 0, max = 2 * pi)
v <- sin(x) + rnorm(n) * 0.25
w <- cos(x) + rnorm(n) * 0.25
z <- 3 * v^2 - w + rnorm(n) * 0.25
s <- z^2 + rnorm(n) * 0.25

data_nonlinear <- data.frame(x = x, v = v, w = w, z = z, s = s)
```

If we try to use the PC algorithm with Fisher’s Z test again it will not
perform well due to the non-linear relationships in the data.

``` r
pc_pcalg_nonlinear <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
pc_result_nonlinear <- disco(data_nonlinear, method = pc_pcalg_nonlinear)
plot(pc_result_nonlinear, main = "PC (pcalg) non-linear")
```

![](causal-discovery_files/figure-html/pc%20algorithm%20non-linear-1.png)

As expected, the PC algorithm with Fisher’s Z test does not recover the
correct causal structure in this non-linear setting at all. Note, that
increasing the sample size does not help.

To handle non-linear relationships, we can for instance use the Kernel
Conditional Independence Test (KCI) in Tetrad.

``` r
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  pc_tetrad_nonlinear <- pc(engine = "tetrad", test = "kci")
  pc_result_nonlinear_kci <- disco(data_nonlinear, method = pc_tetrad_nonlinear)
  plot(pc_result_nonlinear_kci, main = "PC (Tetrad KCI) non-linear")
}
```

![](nonlinear-tetrad-kci.png)

The result of the PC algorithm using the KCI test look like what we’d
expect to see. Note, that this test is much more computationally
demanding than using Fisher’s Z test.

## Incorporating prior knowledge

As the dimensional grows the problem of finding the corresponding graph
given observational data becomes exponentially harder. Thus, if there is
any prior information available, this can help make the problem easier.
We show how to incorporate prior knowledge into the causal discovery
process using the same linear data as above. Suppose we knew that `v`
and `w` do not cause `x`. This can be specified as follows:

``` r
kn <- knowledge(
  data_linear,
  v %!-->% x,  # v does not cause x
  w %!-->% x   # w does not cause x
)
plot(kn)
```

![](causal-discovery_files/figure-html/prior%20knowledge-1.png)

We can then incorporate this knowledge into the PC algorithm as follows:

``` r
pc_pcalg <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
pc_result_with_knowledge <- disco(data_linear, method = pc_pcalg, knowledge = kn)
plot(pc_result_with_knowledge$caugi)
```

![](causal-discovery_files/figure-html/pc%20algorithm%20with%20knowledge-1.png)

It now correctly recovers the true DAG structure with this extra
knowledge.

For more information about how to incorporate knowledge, see the
[knowledge
vignette](https://bjarkehautop.github.io/causalDisco/articles/knowledge.md).
