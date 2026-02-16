# Register a New Tetrad Algorithm

Registers a new Tetrad algorithm by adding it to the internal registry.
The `setup_fun()` should be a function that takes the same arguments as
the runner function for the algorithm and sets up the Tetrad search
object accordingly. This allows you to extend the set of Tetrad
algorithms that can be used with causalDisco.

## Usage

``` r
register_tetrad_algorithm(name, setup_fun)
```

## Arguments

- name:

  Algorithm name (string)

- setup_fun:

  A function that sets up the Tetrad search object for the algorithm. It
  should take the same arguments as the runner function for the
  algorithm.

## See also

Other Extending causalDisco:
[`distribute_engine_args()`](https://disco-coders.github.io/causalDisco/reference/distribute_engine_args.md),
[`new_disco_method()`](https://disco-coders.github.io/causalDisco/reference/new_disco_method.md),
[`reset_tetrad_alg_registry()`](https://disco-coders.github.io/causalDisco/reference/reset_tetrad_alg_registry.md)
