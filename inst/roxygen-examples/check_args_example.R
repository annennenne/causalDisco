### check_and_distribute_args() example ###

# Internal function. Examples only provided for developers extending
# causalDisco.

# Here exemplified through the pc algorithm.

# --- pcalg engine -------------------------------------------------------------

# Valid arguments for pcalg::pc (distributed to the algorithm)
pc_args_ok <- list(skel.method = "stable", numCores = 1, verbose = FALSE)

dist_pcalg <- causalDisco:::check_args_and_distribute_args(
  search = NULL,
  args   = pc_args_ok,
  engine = "pcalg",
  alg    = "pc",
  test   = "gaussCItest"
)

# Inspect what will be forwarded
dist_pcalg$alg_args
dist_pcalg$score_args # pcalg PC doesn't forward score args by default

# Intentional error: unknown argument for pcalg::pc
pc_args_bad <- list(foo = "bar")
try(
  causalDisco:::check_args_and_distribute_args(
    search = NULL,
    args   = pc_args_bad,
    engine = "pcalg",
    alg    = "pc",
    test   = "gaussCItest"
  )
)

# --- bnlearn engine -----------------------------------------------------------

# Valid arguments for bnlearn::pc.stable
bn_args_ok <- list(alpha = 0.05, debug = FALSE)

dist_bn <- causalDisco:::check_args_and_distribute_args(
  search = NULL,
  args   = bn_args_ok,
  engine = "bnlearn",
  alg    = "pc.stable"
)

# The bnlearn checker returns the validated args (no distribution needed)
dist_bn

# Intentional error: unknown argument for bnlearn::pc.stable
bn_args_bad <- list(does_not_exist = TRUE)
try(
  causalDisco:::check_args_and_distribute_args(
    search = NULL,
    args   = bn_args_bad,
    engine = "bnlearn",
    alg    = "pc.stable"
  ),
  silent = TRUE
)

# --- tetrad engine (run only if rJava is available) ---------------------------
# This block is skipped on systems without rJava to keep examples CRAN-safe.

if (requireNamespace("rJava", quietly = TRUE)) {
  if (!isTRUE(rJava::.jniInitialized)) {
    # Initialize JVM if needed (no-op if already initialized)
    init_java()
  }

  # Minimal Tetrad setup: build a search object and distribute PC args.
  search_tetrad <- TetradSearch$new()

  # Example with a common Tetrad CI test;
  # adjust if you use a different test name.
  dist_tetrad <- causalDisco:::check_args_and_distribute_args(
    search = search_tetrad,
    args   = list(verbose = TRUE), # forwarded to search$set_verbose()
    engine = "tetrad",
    alg    = "pc",
    test   = "fisher_z"
  )

  # Inspect distributed arguments to alg/test/score
  dist_tetrad$alg_args
  dist_tetrad$test_args
  dist_tetrad$score_args

  # Intentional error: pass an argument that Tetrad doesn't accept
  try(
    causalDisco:::check_args_and_distribute_args(
      search = search_tetrad,
      args   = list(not_a_param = 123),
      engine = "tetrad",
      alg    = "pc",
      test   = "fisher-z"
    ),
    silent = TRUE
  )
}
