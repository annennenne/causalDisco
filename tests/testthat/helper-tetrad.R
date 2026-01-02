# skip the whole test if Java/rJava/Tetrad jars aren't ready
skip_if_no_tetrad <- function() {
  if (is.null(check_tetrad_install()$version)) {
    testthat::skip("Tetrad not installed or version unknown")
  }

  if (!check_tetrad_install()$java_ok) {
    testthat::skip("Java version not sufficient for Tetrad (need >= 21)")
  }

  ok <- tryCatch(
    {
      init_java()
      TRUE
    },
    error = function(e) FALSE
  )

  if (!ok) {
    testthat::skip(
      "Java/Tetrad not initialized (init_java() failed or no JARs found)"
    )
  }
}

# deterministic tiny continuous dataset with causal structure
make_cont_test_data <- function(n = 300, seed = 1) {
  withr::local_seed(seed)

  # base noise terms
  e1 <- stats::rnorm(n, mean = 0, sd = 1)
  e2 <- stats::rnorm(n, mean = 0, sd = 1)
  e3 <- stats::rnorm(n, mean = 0, sd = 1)
  e4 <- stats::rnorm(n, mean = 0, sd = 1)
  e5 <- stats::rnorm(n, mean = 0, sd = 1)

  # structural equations
  X1 <- e1
  X2 <- 0.8 * X1 + e2
  X3 <- 0.6 * X1 + 0.6 * X2 + e3
  X4 <- -0.5 * X2 + 0.7 * X3 + e4
  X5 <- 0.5 * X4 + e5

  df <- data.frame(X1, X2, X3, X4, X5)

  # standardize each column (mean 0, sd 1)
  df[] <- lapply(df, scale)
  as.data.frame(df)
}

# deterministic tiny discrete dataset with causal structure
make_disc_test_data <- function(n = 300, k = 3, seed = 2) {
  withr::local_seed(seed)

  # latent continuous vars with structure
  cdat <- make_cont_test_data(n = n, seed = seed)

  # discretize into k roughly equal-frequency bins
  disc <- lapply(cdat, function(col) {
    cut(
      col[, 1], # scale() returns a matrix column
      breaks = quantile(col, probs = seq(0, 1, length.out = k + 1)),
      include.lowest = TRUE,
      labels = FALSE
    ) -
      1L # zero-based integers
  })

  as.data.frame(disc) |>
    rlang::set_names(paste0("X", seq_len(5)))
}

# make knowledge object that respects the data-generating mechanism
make_knowledge_test_object <- function(df) {
  tiered_kn <- knowledge(
    df,
    tier(
      1 ~ X1,
      2 ~ X2 + X3,
      3 ~ X4,
      4 ~ X5
    )
  )
  # forbid a tier violation
  forbidden_kn <- knowledge(
    df,
    X2 %--x% X1
  )
  required_kn <- knowledge(
    df,
    X1 %-->% X2
  )
  combi_kn <- tiered_kn + forbidden_kn + required_kn

  list(
    tiered_kn = tiered_kn,
    forbidden_kn = forbidden_kn,
    required_kn = required_kn,
    combi_kn = combi_kn
  )
}

# small assertion helper for Java objects
expect_jobj <- function(x) {
  testthat::expect_s4_class(x, "jobjRef")
}
