# ──────────────────────────────────────────────────────────────────────────────
# Skip if these packages are not installed
# ──────────────────────────────────────────────────────────────────────────────

skip_if_not_installed("pcalg")
skip_if_not_installed("gtools")

# ──────────────────────────────────────────────────────────────────────────────
# Build helper function for knowledge from example data
# ──────────────────────────────────────────────────────────────────────────────

build_kn_from_order <- function() {
  knowledge(
    tpcExample,
    tier(
      child ~ child_x2 + child_x1,
      youth ~ youth_x4 + youth_x3,
      oldage ~ oldage_x6 + oldage_x5
    )
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# tfci_run()
# ──────────────────────────────────────────────────────────────────────────────

test_that("tfci_run returns knowledgeable_caugi on example data", {
  set.seed(123)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  res <- tfci_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.01,
    test = corTest,
    methodOri = "conservative"
  )

  expect_s3_class(res, "knowledgeable_caugi")
})

test_that("tfci_run works with regTest as well", {
  set.seed(777)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  res <- tfci_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.02,
    test = regTest,
    methodOri = "standard"
  )

  expect_s3_class(res, "knowledgeable_caugi")
})

test_that("tfci_run respects forbidden knowledge (edge is removed)", {
  set.seed(999)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  vars <- names(tpcExample)
  x <- vars[1]
  y <- vars[2]
  kn_forb <- kn |> forbid_edge(!!as.name(x) ~ !!as.name(y))

  res <- tfci_run(
    data = tpcExample,
    knowledge = kn_forb,
    alpha = 0.02,
    test = corTest
  )

  expect_s3_class(res, "knowledgeable_caugi")
})

test_that("tfci_run(order=...) runs and returns knowledgeable_caugi, throws deprecation warning", {
  set.seed(202)
  data(tpcExample, package = "causalDisco")

  ord <- c("child", "youth", "oldage")

  expect_warning(
    res <- tfci_run(
      data = tpcExample,
      order = ord,
      alpha = 0.01,
      test = corTest
    )
  )
  expect_s3_class(res, "knowledgeable_caugi")
})


test_that("tfci_run uses provided suffStat (no data needed) and completes", {
  set.seed(2)
  df <- data.frame(
    p1_A = rnorm(25),
    p2_B = rnorm(25)
  )
  # Build a valid knowledge with both variables known
  kn <- knowledge(
    df,
    tier(
      p1 ~ tidyselect::starts_with("p1"),
      p2 ~ tidyselect::starts_with("p2")
    )
  )

  # Provide suffStat directly to hit the else-branch
  ss <- make_suff_stat(df, type = "regTest")

  out <- tfci_run(
    data = NULL, # no data path
    knowledge = kn,
    alpha = 0.2,
    test = regTest,
    suffStat = ss,
    varnames = names(df)
  )

  expect_s3_class(out, "knowledgeable_caugi")
})


# ──────────────────────────────────────────────────────────────────────────────
# tfci_run() guards and errors
# ──────────────────────────────────────────────────────────────────────────────

test_that("tfci_run errors when both knowledge and order are supplied", {
  set.seed(606)
  data(tpcExample, package = "causalDisco")

  ord <- c("child", "youth", "oldage")

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ tidyselect::starts_with("child"),
      youth ~ tidyselect::starts_with("youth"),
      oldage ~ tidyselect::starts_with("oldage")
    )
  )

  expect_error(
    tfci_run(
      data = tpcExample,
      knowledge = kn,
      order = ord,
      alpha = 0.015,
      test = corTest
    ),
    "Both `knowledge` and `order` supplied. Please supply a knowledge object.",
    fixed = TRUE
  )
})

test_that("tfci_run input guards fail fast with clear messages", {
  df <- data.frame(a = 1:3, b = 1:3)
  kn <- knowledge() |> add_vars(names(df))

  expect_error(
    tfci_run(data = NULL, suffStat = NULL, knowledge = knowledge()),
    "Either data or sufficient statistic must be supplied.",
    fixed = TRUE
  )

  expect_error(
    tfci_run(data = df, knowledge = kn, methodNA = "oops"),
    "Invalid choice of method for handling NA values.",
    fixed = TRUE
  )

  expect_error(
    tfci_run(data = df, knowledge = kn, methodOri = "funky"),
    "Orientation method must be one of standard, conservative or maj.rule.",
    fixed = TRUE
  )
})

test_that("tfci_run NA handling: error on NAs with methodNA = 'none', cc with zero rows", {
  df1 <- data.frame(a = c(1, NA), b = c(2, NA))
  kn1 <- knowledge() |> add_vars(names(df1))

  expect_error(
    tfci_run(data = df1, knowledge = kn1, methodNA = "none"),
    "Inputted data contain NA values, but no method for handling missing NAs was supplied.",
    fixed = TRUE
  )

  df2 <- data.frame(a = c(NA, NA), b = c(NA, NA))
  kn2 <- knowledge() |> add_vars(names(df2))

  expect_error(
    tfci_run(data = df2, knowledge = kn2, methodNA = "cc"),
    "contain no complete cases.",
    fixed = TRUE
  )
})

test_that("tfci_run errors when varnames are unknown with suffStat-only usage", {
  suff <- list(dummy = TRUE)
  expect_error(
    tfci_run(data = NULL, suffStat = suff, knowledge = knowledge(), varnames = NULL),
    "Could not determine variable names. Supply `data` or `varnames`.",
    fixed = TRUE
  )
})

test_that("tfci_run demands suffStat for non-builtin test functions", {
  set.seed(1)
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  kn <- knowledge() |> add_vars(names(df))
  strange_test <- function(x, y, S, suffStat) 0

  expect_error(
    tfci_run(data = df, knowledge = kn, test = strange_test),
    "suffStat needs to be supplied when using a non-builtin test.",
    fixed = TRUE
  )
})

test_that("tfci_run() adds missing vars to knowledge via add_vars() and fails for bad variables", {
  skip_if_not_installed("pcalg")
  skip_if_not_installed("gtools")

  # Provide knowledge missing one variable ("oldage_z")
  kn0 <- knowledge() |> add_vars(c("child_x1", "youth_x3"))

  res <- tfci_run(
    data = tpcExample,
    knowledge = kn0, # <- triggers the missing_vars path
    alpha = 0.2,
    test = regTest
  )
  expect_s3_class(res, "knowledgeable_caugi")
  kn_bad <- knowledge() |> add_vars(c("child_a"))
  expect_error(
    tfci_run(
      data = tpcExample,
      knowledge = kn_bad,
      alpha = 0.2,
      test = regTest
    ),
    "Knowledge contains variables not present in `data`: child_a",
    fixed = TRUE
  )
})
