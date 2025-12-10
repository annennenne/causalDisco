# ──────────────────────────────────────────────────────────────────────────────
# Helper functions for tests
# ──────────────────────────────────────────────────────────────────────────────

ges_registry <- list(
  ges = list(fn = ges, engines = c("tetrad", "pcalg"))
)

ges_args <- function(engine) {
  if (engine == "pcalg") {
    list(score = "sem_bic", directed_as_undirected_knowledge = TRUE)
  } else {
    list(score = "sem_bic")
  }
}

toy_df <- function(n = 100) {
  set.seed(7)
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
# ges()
# ──────────────────────────────────────────────────────────────────────────────

test_that("ges(): constructor returns a disco_method and runs across engines", {
  skip_if_no_tetrad()
  df <- toy_df()

  for (engine in ges_registry$ges$engines) {
    m <- do.call(ges_registry$ges$fn, c(list(engine = engine), ges_args(engine)))

    expect_s3_class(m, c("ges", "disco_method", "function"))
    expect_error(m(1:3), "`data` must be a data frame.", fixed = TRUE)

    res <- m(df)
    expect_s3_class(res, "knowledgeable_caugi")
  }
})

test_that("ges(): set_knowledge returns a new method and injects knowledge", {
  skip_if_no_tetrad()
  df <- toy_df()
  kn <- toy_knowledge(df)

  for (engine in ges_registry$ges$engines) {
    m <- do.call(ges_registry$ges$fn, c(list(engine = engine), ges_args(engine)))

    res0 <- m(df)
    expect_s3_class(res0, "knowledgeable_caugi")

    m2 <- set_knowledge(m, kn)
    expect_s3_class(m2, c("ges", "disco_method", "function"))

    if (engine == "pcalg") {
      expect_warning(
        m2(df),
        "pcalg::ges() does not take required edges as arguments.\n  They will not be used here.",
        fixed = TRUE
      )
    } else {
      expect_s3_class(m2(df), "knowledgeable_caugi")
    }

    expect_s3_class(m(df), "knowledgeable_caugi")
  }
})

test_that("ges(): disco() injects knowledge and validates method type", {
  skip_if_no_tetrad()
  df <- toy_df()
  kn <- toy_knowledge(df)

  expect_error(
    disco(df, method = function(x) x),
    "The method must be a disco method object.",
    fixed = TRUE
  )

  for (engine in ges_registry$ges$engines) {
    m <- do.call(ges_registry$ges$fn, c(list(engine = engine), ges_args(engine)))

    if (engine == "pcalg") {
      expect_warning(
        disco(df, method = m, knowledge = kn),
        "pcalg::ges() does not take required edges as arguments.\n  They will not be used here.",
        fixed = TRUE
      )
    } else {
      res <- disco(df, method = m, knowledge = kn)
      expect_s3_class(res, "knowledgeable_caugi")
    }
  }
})

test_that("ges(): disco() forwards knowledge errors from set_knowledge()", {
  skip_if_no_tetrad()
  df <- toy_df()

  for (engine in ges_registry$ges$engines) {
    m <- do.call(ges_registry$ges$fn, c(list(engine = engine), ges_args(engine)))

    expect_error(
      disco(df, method = m, knowledge = list(foo = "bar")),
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
  df <- toy_df()

  # tetrad
  runner_t <- ges_tetrad_runner(
    score = "sem_bic"
  )
  expect_type(runner_t, "list")
  expect_true(is.function(runner_t$run))
  expect_s3_class(runner_t$run(df), "knowledgeable_caugi")

  # also with more arguments
  runner_t2 <- ges_tetrad_runner(
    score = "sem_bic", symmetric_first_step = TRUE,
    singularity_lambda = 0.1
  )
  expect_type(runner_t2, "list")
  expect_true(is.function(runner_t2$run))
  expect_s3_class(runner_t2$run(df), "knowledgeable_caugi")

  # pcalg
  runner_p <- ges_pcalg_runner(score = "sem_bic", directed_as_undirected_knowledge = TRUE)
  expect_type(runner_p, "list")
  expect_true(is.function(runner_p$run))
  expect_s3_class(runner_p$run(df), "knowledgeable_caugi")
})
