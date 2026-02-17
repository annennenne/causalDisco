# Extending causalDisco with New Algorithms

``` r
library(causalDisco)
#> causalDisco startup:
#>   Java heap size requested: 2 GB
#>   Tetrad version: 7.6.10
#>   Java successfully initialized with 2 GB.
#>   To change heap size, set options(java.heap.size = 'Ng') or Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.
#>   Restart R to apply changes.
```

This vignette illustrates how to add a new algorithm to `causalDisco`
from bnlearn, pcalg and Tetrad. Both bnlearn and pcalg are R packages
and therefore share the same interface, while Tetrad is a Java
application and requires a slightly different integration pattern.

## bnlearn and pcalg

Suppose we want to add the Hybrid Parents and Children (HPC) algorithm
from bnlearn. We can integrate it into causalDisco by creating a new
function `hpc()`.

HPC is a constraint-based algorithm, so it requires a conditional
independence test and a significance level alpha. It returns a PDAG, so
we set the `graph_class` attribute to `"PDAG"`.

The function takes an `engine` argument, which specifies which
implementation of the algorithm to use. Even though we only add HPC from
bnlearn in this example, we set up the structure to allow for multiple
engines in the future. The `...` argument allows us to pass additional
arguments to the underlying algorithm and test.

The code for the `hpc()` function is as follows:

``` r
hpc <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        hpc_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }
  method <- new_disco_method(
    builder = builder,
    name = "hpc",
    engine = engine,
    graph_class = "PDAG"
  )
  method
}
```

We see that the `hpc()` function needs two components:

1.  **Builder function:** Constructs a *runner* for the algorithm. It
    configures the algorithm with the provided parameters and optional
    background knowledge.

2.  **Method object:** Created via
    [`new_disco_method()`](https://disco-coders.github.io/causalDisco/reference/new_disco_method.md).
    It encapsulates the builder and metadata about the method. This
    allows us to be able to call
    [`disco()`](https://disco-coders.github.io/causalDisco/reference/disco.md)
    to execute the algorithm on a dataset.

Next, we define the runner. The runner handles configuring the search
object with the chosen test, alpha level, and any additional arguments,
and it actually runs the search on the data when called. For bnlearn
algorithms, we use the `BnlearnSearch` R6 class to manage search
configuration and execution. We also use
[`distribute_engine_args()`](https://disco-coders.github.io/causalDisco/reference/distribute_engine_args.md)
to determine which extra arguments should be passed to the test, the
algorithm, or both.

``` r
hpc_bnlearn_runner <- function(test, alpha, ...) {
  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- distribute_engine_args(
    search = search,
    args = args,
    engine = "bnlearn",
    alg = "hpc"
  )

  search$set_test(test, alpha)
  search$set_alg("hpc", args_to_pass)

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}
```

Once defined, the HPC algorithm can be used like any other method in
causalDisco. We first construct the method using `hpc()`, and then pass
it to
[`disco()`](https://disco-coders.github.io/causalDisco/reference/disco.md).
Here we demonstrate using the included `tpc_example` dataset:

``` r
data(tpc_example)
hpc_bnlearn <- hpc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
hpc_bnlearn_result <- disco(tpc_example, hpc_bnlearn)
plot(hpc_bnlearn_result)
```

![](new-algorithm_files/figure-html/hpc%20example-1.png)

To implement a **score-based** algorithm instead, the structure would
remain the same. The main difference is that the method would accept a
`score` argument rather than `test` and `alpha`, and the runner would
call `search$set_score()` instead of `search$set_test()`. A **hybrid**
algorithm algorithm would accept `test`, `alpha`, and `score` arguments,
and the runner would call both `set_test()` and `set_score()`.

To implement an algorithm from pcalg rather than bnlearn, we would
follow the same structure but use the R6 class `PcalgSearch` instead of
`BnlearnSearch` in the runner (i.e., `PcalgSearch$new()`).

## Tetrad

We will illustrate how to make a custom version (which actually does the
same as our implementation, just with some defensive programming
omitted) of the Boss algorithm from Tetrad. Tetrad algorithms follow a
slightly different integration pattern because Tetrad is a Java
application rather than an R package.

To add a new Tetrad algorithm, first register it with
[`register_tetrad_algorithm()`](https://disco-coders.github.io/causalDisco/reference/register_tetrad_algorithm.md).
This function requires:

- The algorithm name.
- A setup function that configures a `TetradSearch` object to execute
  the algorithm.

The setup function receives the TetradSearch object as its first
argument, along with any additional parameters passed via `...` when the
method is constructed. Its responsibilities are to:

1.  Configure the TetradSearch object with the appropriate parameters
    and score.
2.  Instantiate the correct Tetrad algorithm class.

The fully qualified class name can be identified by inspecting the
Tetrad source code at:

<https://github.com/cmu-phil/tetrad>

Be sure to browse the version corresponding to the installed Tetrad
release. The relevant Java classes are located under:

<https://github.com/cmu-phil/tetrad/tree/development/tetrad-lib/src/main/java>

``` r
register_tetrad_algorithm(
  "my_boss_variant",
  function(
    self,
    num_starts = 1,
    use_bes = TRUE,
    use_data_order = TRUE,
    output_cpdag = TRUE
  ) {
    self$set_params(
      USE_BES = use_bes,
      NUM_STARTS = num_starts,
      USE_DATA_ORDER = use_data_order,
      OUTPUT_CPDAG = output_cpdag
    )

    self$alg <- rJava::.jnew(
      "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Boss",
      self$score
    )
    self$alg$setKnowledge(self$knowledge)
  }
)
```

You can view all the custom registered Tetrad algorithms using
`tetrad_alg_registry()`, and reset it using
[`reset_tetrad_alg_registry()`](https://disco-coders.github.io/causalDisco/reference/reset_tetrad_alg_registry.md).

Like for bnlearn and pcalg, we then create a method function
`my_boss_variant()` that constructs a builder function which in turn
constructs a runner using the registered Tetrad algorithm.

BOSS is a **score-based** algorithm, so the method accepts a `score`
argument and passes it to the runner, which configures the
`TetradSearch` object accordingly. It returns a PDAG, so we set the
`graph_class` attribute to `"PDAG"`.

``` r
my_boss_variant <- function(
  engine = "tetrad",
  score,
  ...
) {
  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(my_boss_variant_tetrad_runner, score, !!!args)
    )
    runner
  }

  method <- new_disco_method(
    builder = builder,
    name = "my_boss_variant",
    engine = engine,
    graph_class = "PDAG"
  )
  method
}
```

Next we define the runner function `my_boss_variant_tetrad_runner()`

``` r
my_boss_variant_tetrad_runner <- function(score, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- distribute_engine_args(
    search = search,
    args = args,
    engine = "tetrad",
    alg = "my_boss_variant"
  )

  if (length(args_to_pass$score_args) > 0) {
    rlang::exec(search$set_score, score, !!!args_to_pass$score_args)
  } else {
    search$set_score(score)
  }

  if (length(args_to_pass$alg_args) > 0) {
    rlang::exec(search$set_alg, "my_boss_variant", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("my_boss_variant")
  }

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}
```

Once defined we can now run `my_boss_variant()` like any other method in
causalDisco.

``` r
# Ensure Tetrad is installed and Java is working before running the algorithm
if (verify_tetrad()$installed && verify_tetrad()$java_ok) {
  my_boss_variant_tetrad <- my_boss_variant(engine = "tetrad", score = "sem_bic")
  my_boss_variant_tetrad_result <- disco(tpc_example, my_boss_variant_tetrad)
  plot(my_boss_variant_tetrad_result)
}
```

![](custom-boss-variant.png)

Once again, if using a hybrid or constraint-based algorithm rather than
a score-based one, the structure would remain the same but the method
would accept different arguments (e.g., `test` and `alpha` for a
constraint-based algorithm).

## Conclusion

In this vignette, we have illustrated how to add new algorithms to
causalDisco, both from the R packages pcalg and bnlearn and from Tetrad.

If you have any feedback or suggestions for improvement on the API and
functionality for extending causalDisco, please let us know by opening
an issue on GitHub.
