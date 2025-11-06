# ──────────────────────────────────────────────────────────────────────────────
# Helpers for testing constraint-based methods (pc, fci) across engines
# ──────────────────────────────────────────────────────────────────────────────

# register methods and the engines they support
method_registry <- list(
  pc  = list(fn = pc, engines = c("tetrad", "pcalg", "bnlearn")),
  fci = list(fn = fci, engines = c("tetrad", "pcalg"))
)

# per-method arguments (add branches here if a method/engine needs extras)
method_args <- function(method_name, engine) {
  args <- list(test = "fisher_z", alpha = 0.05)
  if (engine == "pcalg") {
    args$directed_as_undirected_knowledge <- TRUE
  }
  args
}

skip_if_no_pcalg_bnlearn_tetrad <- function() {
  skip_if_no_tetrad()
  skip_if_not_installed("pcalg")
  skip_if_not_installed("bnlearn")
}

# tiny continuous dataset; fast & stable
toy_df <- function(n = 60L) {
  set.seed(69)
  V1 <- rnorm(n)
  V3 <- rnorm(n, 0, 0.2)
  V2 <- 0.6 * V1 + 0.4 * V3 + rnorm(n, 0, 0.05)
  V4 <- V3 + rnorm(n)
  V5 <- V3 + rnorm(n)
  V6 <- 0.7 * V5 + rnorm(n, 0, 0.1)
  data.frame(V1, V2, V3, V4, V5, V6)
}

toy_knowledge <- function(df) {
  knowledge(
    df,
    required(
      V1 ~ V2,
      V5 ~ V6
    )
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# Tests for constraint-based methods (pc, fci) across engines
# ──────────────────────────────────────────────────────────────────────────────

test_that("methods construct disco_method closures and run across engines", {
  skip_if_no_pcalg_bnlearn_tetrad()
  df <- toy_df()

  for (method_name in names(method_registry)) {
    reg <- method_registry[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      expect_s3_class(m, c(method_name, "disco_method", "function"))
      expect_error(m(1:3), "`data` must be a data frame.", fixed = TRUE)

      res <- m(df)
      expect_s3_class(res, "knowledgeable_caugi")
    }
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# set_knowledge()
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_knowledge returns a new method and injects knowledge (all engines)", {
  skip_if_no_pcalg_bnlearn_tetrad()
  df <- toy_df()
  kn <- toy_knowledge(df)

  for (method_name in names(method_registry)) {
    reg <- method_registry[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      res0 <- m(df)
      expect_s3_class(res0, "knowledgeable_caugi")

      m2 <- set_knowledge(m, kn)
      expect_s3_class(m2, c(method_name, "disco_method", "function"))
      expect_s3_class(m2(df), "knowledgeable_caugi")
      expect_s3_class(m(df), "knowledgeable_caugi")
    }
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# disco()
# ──────────────────────────────────────────────────────────────────────────────

test_that("disco() injects knowledge and validates method type (pc + fci)", {
  skip_if_no_pcalg_bnlearn_tetrad()
  df <- toy_df()
  kn <- toy_knowledge(df)

  expect_error(
    disco(df, method = function(x) x),
    "The method must be a disco method object.",
    fixed = TRUE
  )

  for (method_name in names(method_registry)) {
    reg <- method_registry[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      res <- disco(df, method = m, knowledge = kn)
      expect_s3_class(res, "knowledgeable_caugi")
    }
  }
})

test_that("disco() forwards knowledge errors from set_knowledge() (pc + fci)", {
  skip_if_no_pcalg_bnlearn_tetrad()
  df <- toy_df()

  for (method_name in names(method_registry)) {
    reg <- method_registry[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      expect_error(
        disco(df, method = m, knowledge = list(foo = "bar")),
        "Input must be a knowledge instance.",
        fixed = TRUE
      )
    }
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# Direct runner tests (pc + fci)
# ──────────────────────────────────────────────────────────────────────────────

test_that("pc and fci runners wire arguments correctly for each engine", {
  skip_if_no_pcalg_bnlearn_tetrad()
  df <- toy_df()

  # pc: tetrad (incl. extra test/alg params)
  runner_t_pc <- pc_tetrad_runner(test = "fisher_z", alpha = 0.05)
  expect_type(runner_t_pc, "list")
  expect_true(is.function(runner_t_pc$run))
  expect_s3_class(runner_t_pc$run(df), "knowledgeable_caugi")

  runner_t_pc2 <- pc_tetrad_runner(
    test = "fisher_z", alpha = 0.05,
    singularity_lambda = 0.1, guarantee_cpdag = TRUE
  )
  expect_type(runner_t_pc2, "list")
  expect_true(is.function(runner_t_pc2$run))
  expect_s3_class(runner_t_pc2$run(df), "knowledgeable_caugi")

  # pc: pcalg (+ alg args path via m.max)
  runner_p_pc <- pc_pcalg_runner(
    test = "fisher_z", alpha = 0.05, m.max = 1L,
    directed_as_undirected_knowledge = TRUE
  )
  expect_type(runner_p_pc, "list")
  expect_true(is.function(runner_p_pc$run))
  expect_s3_class(runner_p_pc$run(df), "knowledgeable_caugi")

  # pc: bnlearn
  runner_b_pc <- pc_bnlearn_runner(test = "fisher_z", alpha = 0.05)
  expect_type(runner_b_pc, "list")
  expect_true(is.function(runner_b_pc$run))
  expect_s3_class(runner_b_pc$run(df), "knowledgeable_caugi")

  # fci: tetrad (incl. extra test/alg params)
  runner_t_fci <- fci_tetrad_runner(test = "fisher_z", alpha = 0.05)
  expect_type(runner_t_fci, "list")
  expect_true(is.function(runner_t_fci$run))
  expect_s3_class(runner_t_fci$run(df), "knowledgeable_caugi")

  runner_t_fci2 <- fci_tetrad_runner(
    test = "fisher_z", alpha = 0.05,
    singularity_lambda = 0.1, depth = 2L
  )
  expect_type(runner_t_fci2, "list")
  expect_true(is.function(runner_t_fci2$run))
  expect_s3_class(runner_t_fci2$run(df), "knowledgeable_caugi")

  # fci: pcalg (+ alg args path)
  runner_p_fci <- fci_pcalg_runner(
    test = "fisher_z", alpha = 0.05, m.max = 1L,
    directed_as_undirected_knowledge = TRUE
  )
  expect_type(runner_p_fci, "list")
  expect_true(is.function(runner_p_fci$run))
  expect_s3_class(runner_p_fci$run(df), "knowledgeable_caugi")
})
