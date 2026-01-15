# Temporal EssGraph class with greedy steps

A `RefClass` extending `EssGraph` that exposes a single-step greedy move
(`forward`, `backward`, or `turning`) through `greedy_step()`. Used by
[tges_run](https://bjarkehautop.github.io/causalDisco/reference/tges_run.md)
to iterate GIES one step at a time and interleave background knowledge
enforcement.

## Methods

`greedy_step(direction = c("forward", "backward", "turning"), verbose = FALSE, ...)`

- `direction` Character; one of `"forward"`, `"backward"`, or
  `"turning"`, indicating which phase of GIES to perform a single step
  of.

- `verbose` Logical; indicates whether debug output should be printed.

- `...` Additional arguments passed to the underlying C++ function
  causalInference from pcalg.

## See also

[tges_run](https://bjarkehautop.github.io/causalDisco/reference/tges_run.md),
[TemporalBIC](https://bjarkehautop.github.io/causalDisco/reference/TemporalBIC-class.md),
[TemporalBDeu](https://bjarkehautop.github.io/causalDisco/reference/TemporalBDeu-class.md)
