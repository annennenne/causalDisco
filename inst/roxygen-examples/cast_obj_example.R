### cast_obj() example ###

# This is an example of how to use the cast_obj() function to cast a Java object
# The function is internal and is used in the TetradSearch class.

# run only if rJava is available; use try() to avoid noisy failures.
if (requireNamespace("rJava", quietly = TRUE)) {
  # Initialize JVM if needed (no-op if already initialized)
  if (!isTRUE(rJava::.jniInitialized)) {
    try(causalDisco:::init_java())
  }

  # Example 1 — cast a Tetrad Graph
  g <- try(rJava::.jnew("edu/cmu/tetrad/graph/EdgeListGraph"))
  if (!inherits(g, "try-error")) {
    invisible(try(causalDisco:::cast_obj(g)))
  }

  # Example 2 — negative control: cannot be cast
  s <- try(rJava::.jnew("java/lang/String", "hello"))
  if (!inherits(s, "try-error")) {
    invisible(try(causalDisco:::cast_obj(s)))
  }
}
