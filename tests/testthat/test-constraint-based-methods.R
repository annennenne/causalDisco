# ──────────────────────────────────────────────────────────────────────────────
# Tests for constraint-based methods (pc, fci) across engines
# ──────────────────────────────────────────────────────────────────────────────

test_that("methods construct disco_method closures and run across engines", {
  skip_if_no_tetrad()
  data(num_data)

  for (method_name in names(method_registry_constraint)) {
    reg <- method_registry_constraint[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      expect_s3_class(m, c(method_name, "disco_method", "function"))
      expect_error(m(1:3), "`data` must be a data frame.", fixed = TRUE)

      res <- m(num_data)
      expect_s3_class(res, "disco")
    }
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# set_knowledge()
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_knowledge returns a new method and injects knowledge (all engines)", {
  skip_if_no_tetrad()
  data(num_data)
  kn <- toy_knowledge(num_data)

  for (method_name in names(method_registry_constraint)) {
    reg <- method_registry_constraint[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      res0 <- m(num_data)
      expect_s3_class(res0, "disco")

      m2 <- set_knowledge(m, kn)
      expect_s3_class(m2, c(method_name, "disco_method", "function"))
      if (engine == "pcalg") {
        expect_warning(
          m2(num_data),
          "Engine pcalg does not use required edges; ignoring them.",
          fixed = TRUE
        )
      } else {
        expect_s3_class(m2(num_data), "disco")
      }
      expect_s3_class(m(num_data), "disco")
    }
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# disco()
# ──────────────────────────────────────────────────────────────────────────────

test_that("disco() injects knowledge and validates method type (pc + fci)", {
  skip_if_no_tetrad()
  data(num_data)
  kn <- toy_knowledge(num_data)

  expect_error(
    disco(num_data, method = function(x) x),
    "The method must be a disco method object.",
    fixed = TRUE
  )
  for (method_name in names(method_registry_constraint)) {
    reg <- method_registry_constraint[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      if (engine == "pcalg") {
        expect_warning(
          disco(num_data, method = m, knowledge = kn),
          "Engine pcalg does not use required edges; ignoring them.",
          fixed = TRUE
        )
      } else {
        if (engine == "tetrad" && method_name == "pc") {
          expect_warning(
            {
              res <- disco(num_data, method = m, knowledge = kn)
            },
            "Cannot mutate graph to class 'PDAG'.",
            fixed = TRUE
          )
        } else {
          res <- disco(num_data, method = m, knowledge = kn)
        }
      }
      expect_s3_class(res, "disco")
    }
  }
})

test_that("disco() forwards knowledge errors from set_knowledge() (pc + fci)", {
  skip_if_no_tetrad()
  data(num_data)

  for (method_name in names(method_registry_constraint)) {
    reg <- method_registry_constraint[[method_name]]
    for (engine in reg$engines) {
      args <- method_args(method_name, engine)
      m <- do.call(reg$fn, c(list(engine = engine), args))

      expect_error(
        disco(num_data, method = m, knowledge = list(foo = "bar")),
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
  skip_if_no_tetrad()
  data(num_data)

  # pc: Tetrad (incl. extra test/alg params)
  runner_t_pc <- pc_tetrad_runner(test = "fisher_z", alpha = 0.05)
  expect_type(runner_t_pc, "list")
  expect_true(is.function(runner_t_pc$run))
  expect_s3_class(runner_t_pc$run(num_data), "disco")

  runner_t_pc2 <- pc_tetrad_runner(
    test = "fisher_z",
    alpha = 0.05,
    singularity_lambda = 0.1,
    guarantee_cpdag = TRUE
  )
  expect_type(runner_t_pc2, "list")
  expect_true(is.function(runner_t_pc2$run))
  expect_s3_class(runner_t_pc2$run(num_data), "disco")

  # pc: pcalg (+ alg args path via m.max)
  runner_p_pc <- pc_pcalg_runner(
    test = "fisher_z",
    alpha = 0.05,
    m.max = 1L,
    directed_as_undirected_knowledge = TRUE
  )
  expect_type(runner_p_pc, "list")
  expect_true(is.function(runner_p_pc$run))
  expect_s3_class(runner_p_pc$run(num_data), "disco")

  # pc: bnlearn
  runner_b_pc <- pc_bnlearn_runner(test = "fisher_z", alpha = 0.05)
  expect_type(runner_b_pc, "list")
  expect_true(is.function(runner_b_pc$run))
  expect_s3_class(runner_b_pc$run(num_data), "disco")

  # fci: Tetrad (incl. extra test/alg params)
  runner_t_fci <- fci_tetrad_runner(test = "fisher_z", alpha = 0.05)
  expect_type(runner_t_fci, "list")
  expect_true(is.function(runner_t_fci$run))
  expect_s3_class(runner_t_fci$run(num_data), "disco")

  runner_t_fci2 <- fci_tetrad_runner(
    test = "fisher_z",
    alpha = 0.05,
    singularity_lambda = 0.1,
    depth = 2L
  )
  expect_type(runner_t_fci2, "list")
  expect_true(is.function(runner_t_fci2$run))
  expect_s3_class(runner_t_fci2$run(num_data), "disco")

  # fci: pcalg (+ alg args path)
  runner_p_fci <- fci_pcalg_runner(
    test = "fisher_z",
    alpha = 0.05,
    m.max = 1L,
    directed_as_undirected_knowledge = TRUE
  )
  expect_type(runner_p_fci, "list")
  expect_true(is.function(runner_p_fci$run))
  expect_s3_class(runner_p_fci$run(num_data), "disco")
})
