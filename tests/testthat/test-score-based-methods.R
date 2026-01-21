# ──────────────────────────────────────────────────────────────────────────────
# ges()
# ──────────────────────────────────────────────────────────────────────────────

test_that("ges(): constructor returns a disco_method and runs across engines", {
  skip_if_no_tetrad()
  data(num_data)

  for (engine in ges_registry$ges$engines) {
    m <- do.call(
      ges_registry$ges$fn,
      c(list(engine = engine), ges_args(engine))
    )

    expect_s3_class(m, c("ges", "disco_method", "function"))
    expect_error(m(1:3), "`data` must be a data frame.", fixed = TRUE)

    res <- m(num_data)
    expect_s3_class(res, "knowledgeable_caugi")
  }
})

test_that("ges(): set_knowledge returns a new method and injects knowledge", {
  skip_if_no_tetrad()
  data(num_data)
  kn <- toy_knowledge(num_data)

  for (engine in ges_registry$ges$engines) {
    m <- do.call(
      ges_registry$ges$fn,
      c(list(engine = engine), ges_args(engine))
    )

    res0 <- m(num_data)
    expect_s3_class(res0, "knowledgeable_caugi")

    m2 <- set_knowledge(m, kn)
    expect_s3_class(m2, c("ges", "disco_method", "function"))

    if (engine == "pcalg") {
      expect_warning(
        m2(num_data),
        "Engine pcalg does not use required edges; ignoring them.",
        fixed = TRUE
      )
    } else {
      expect_s3_class(m2(num_data), "knowledgeable_caugi")
    }

    expect_s3_class(m(num_data), "knowledgeable_caugi")
  }
})

test_that("ges(): disco() injects knowledge and validates method type", {
  skip_if_no_tetrad()
  data(num_data)
  kn <- toy_knowledge(num_data)

  expect_error(
    disco(num_data, method = function(x) x),
    "The method must be a disco method object.",
    fixed = TRUE
  )

  for (engine in ges_registry$ges$engines) {
    m <- do.call(
      ges_registry$ges$fn,
      c(list(engine = engine), ges_args(engine))
    )

    if (engine == "pcalg") {
      expect_warning(
        disco(num_data, method = m, knowledge = kn),
        "Engine pcalg does not use required edges; ignoring them.",
        fixed = TRUE
      )
    } else {
      res <- disco(num_data, method = m, knowledge = kn)
      expect_s3_class(res, "knowledgeable_caugi")
    }
  }
})

test_that("ges(): disco() forwards knowledge errors from set_knowledge()", {
  skip_if_no_tetrad()
  data(num_data)

  for (engine in ges_registry$ges$engines) {
    m <- do.call(
      ges_registry$ges$fn,
      c(list(engine = engine), ges_args(engine))
    )

    expect_error(
      disco(num_data, method = m, knowledge = list(foo = "bar")),
      "Input must be a knowledge instance.",
      fixed = TRUE
    )
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# Direct testing of the runners
# ──────────────────────────────────────────────────────────────────────────────

test_that("ges runners wire arguments correctly for each engine", {
  skip_if_no_tetrad()
  data(num_data)

  # tetrad
  runner_t <- ges_tetrad_runner(
    score = "sem_bic"
  )
  expect_type(runner_t, "list")
  expect_true(is.function(runner_t$run))
  expect_s3_class(runner_t$run(num_data), "knowledgeable_caugi")

  # also with more arguments
  runner_t2 <- ges_tetrad_runner(
    score = "sem_bic",
    symmetric_first_step = TRUE,
    singularity_lambda = 0.1
  )
  expect_type(runner_t2, "list")
  expect_true(is.function(runner_t2$run))
  expect_s3_class(runner_t2$run(num_data), "knowledgeable_caugi")

  # pcalg
  runner_p <- ges_pcalg_runner(
    score = "sem_bic",
    directed_as_undirected_knowledge = TRUE
  )
  expect_type(runner_p, "list")
  expect_true(is.function(runner_p$run))
  expect_s3_class(runner_p$run(num_data), "knowledgeable_caugi")
})
