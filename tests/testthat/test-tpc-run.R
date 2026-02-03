# ──────────────────────────────────────────────────────────────────────────────
# tpc_run() guards and errors
# ──────────────────────────────────────────────────────────────────────────────

test_that("tpc_run input guards fail fast with clear messages", {
  df <- data.frame(a = 1:3, b = 1:3)
  kn <- knowledge() |> add_vars(names(df))

  expect_error(
    tpc_run(data = df, knowledge = kn, na_method = "oops"),
    "Invalid choice of method for handling NA values.",
    fixed = TRUE
  )
  expect_error(
    tpc_run(data = NULL, suff_stat = NULL, knowledge = knowledge()),
    "Either data or sufficient statistic must be supplied.",
    fixed = TRUE
  )
})

test_that("tpc_run NA handling: error on NAs with na_method = 'none', cc with zero rows", {
  df1 <- data.frame(a = c(1, NA), b = c(2, NA))
  kn1 <- knowledge() |> add_vars(names(df1))

  expect_error(
    tpc_run(data = df1, knowledge = kn1, na_method = "none"),
    "Inputted data contain NA values, but no method for handling missing NAs was supplied.",
    fixed = TRUE
  )

  df2 <- data.frame(a = c(NA, NA), b = c(NA, NA))
  kn2 <- knowledge() |> add_vars(names(df2))

  expect_error(
    tpc_run(data = df2, knowledge = kn2, na_method = "cc"),
    "contain no complete cases.",
    fixed = TRUE
  )
})

test_that("tpc_run errors when varnames are unknown with suff_stat-only usage", {
  suff <- list(dummy = TRUE)
  expect_error(
    tpc_run(
      data = NULL,
      suff_stat = suff,
      knowledge = knowledge(),
      varnames = NULL
    ),
    "Could not determine variable names. Supply `data` or `varnames`.",
    fixed = TRUE
  )
})

test_that("tpc_run demands suff_stat for non-builtin test functions", {
  set.seed(1)
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  kn <- knowledge() |> add_vars(names(df))
  strange_test <- function(x, y, S, suff_stat) 0

  expect_error(
    tpc_run(data = df, knowledge = kn, test = strange_test),
    "suff_stat needs to be supplied when using a non-builtin test.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Helpers: make_suff_stat()
# ──────────────────────────────────────────────────────────────────────────────

test_that("make_suff_stat() returns correct suff_stat for different tests and fails correctly", {
  set.seed(12)
  df <- data.frame(
    child_x = rnorm(40),
    youth_y = rnorm(40),
    oldage_z = rnorm(40)
  )
  suff <- make_suff_stat(df, type = "reg_test")
  expect_true(is.list(suff))
  expect_true(!is.null(suff$data))
  expect_true(!is.null(suff$bin))

  suff2 <- make_suff_stat(df, type = "cor_test")
  expect_true(is.list(suff2))
  expect_true(!is.null(suff2$C))
  expect_true(!is.null(suff2$n))

  expect_error(
    make_suff_stat(df, type = "unknownTest"),
    "unknownTest is not a supported type for autogenerating a sufficient statistic",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Helpers: .build_knowledge_from_order
# ──────────────────────────────────────────────────────────────────────────────

test_that(".build_knowledge_from_order builds tiers in the given order and attaches starts_with() vars", {
  vars <- c("childA", "childB", "youthC", "oldageD")
  df <- data.frame(
    childA = 1:3,
    childB = 1:3,
    youthC = 1:3,
    oldageD = 1:3
  )
  kn <- .build_knowledge_from_order(
    order = c("child", "youth", "oldage"),
    data = df,
    vnames = vars
  )

  expect_s3_class(kn, "knowledge")
  expect_identical(kn$tiers$label, c("child", "youth", "oldage"))
  expect_setequal(kn$vars$var[kn$vars$tier == "child"], c("childA", "childB"))
  expect_setequal(kn$vars$var[kn$vars$tier == "youth"], "youthC")
  expect_setequal(kn$vars$var[kn$vars$tier == "oldage"], "oldageD")
})

test_that(".build_knowledge_from_order returns merged knowledge when data is present", {
  df <- data.frame(child_x = 1:3, youth_y = 1:3, oldage_z = 1:3)
  kn <- .build_knowledge_from_order(
    order = c("child", "youth", "oldage"),
    data = df,
    vnames = NULL
  )

  expect_s3_class(kn, "knowledge")
  expect_identical(kn$tiers$label, c("child", "youth", "oldage"))
  expect_true(all(names(df) %in% kn$vars$var))
})


test_that(".build_knowledge_from_order errors when data is NULL and vnames missing", {
  expect_error(
    .build_knowledge_from_order(
      order = c("T1", "T2"),
      data = NULL,
      vnames = NULL
    ),
    "`data` is NULL, so `vnames` should be provided.",
    fixed = TRUE
  )
})

test_that(".build_knowledge_from_order builds tiers in declared order (vnames path)", {
  vnames <- c("T1_x", "T1_y", "T2_a", "zzz")
  kn <- .build_knowledge_from_order(
    order = c("T1", "T2"),
    data = NULL,
    vnames = vnames
  )

  expect_s3_class(kn, "knowledge")
  expect_identical(kn$tiers$label, c("T1", "T2"))
  expect_setequal(kn$vars$var, vnames)

  # guard against NA in the logical index
  t1_idx <- which(!is.na(kn$vars$tier) & kn$vars$tier == "T1")
  t2_idx <- which(!is.na(kn$vars$tier) & kn$vars$tier == "T2")

  expect_setequal(kn$vars$var[t1_idx], c("T1_x", "T1_y"))
  expect_setequal(kn$vars$var[t2_idx], "T2_a")

  # variables with no matching prefix remain NA
  expect_true(is.na(kn$vars$tier[match("zzz", kn$vars$var)]))
})

test_that(".build_knowledge_from_order does not overwrite earlier tier assignments", {
  # x1a matches both "x" and "x1"; since we declare order = c("x", "x1"),
  # "x" must win and x1a should stay in tier "x"
  vnames <- c("x", "x1a", "x1b", "other")
  kn <- .build_knowledge_from_order(
    order = c("x", "x1"),
    data = NULL,
    vnames = vnames
  )

  expect_identical(kn$tiers$label, c("x", "x1"))

  # x assigned to tier "x"
  expect_identical(kn$vars$tier[match("x", kn$vars$var)], "x")

  # x1a/x1b start with both "x" and "x1"; first hit ("x") should stick
  expect_identical(kn$vars$tier[match("x1a", kn$vars$var)], "x")
  expect_identical(kn$vars$tier[match("x1b", kn$vars$var)], "x")

  # unmatched stays NA
  expect_true(is.na(kn$vars$tier[match("other", kn$vars$var)]))
})

test_that(".build_knowledge_from_order respects order even with empty-hit tiers", {
  # Include a tier label that matches no variables; it should still appear
  vnames <- c("A_1", "B_2")
  kn <- .build_knowledge_from_order(
    order = c("A", "NOHIT", "B"),
    data = NULL,
    vnames = vnames
  )

  expect_identical(kn$tiers$label, c("A", "NOHIT", "B"))
  expect_setequal(kn$vars$var[kn$vars$tier == "A"], "A_1")
  expect_setequal(kn$vars$var[kn$vars$tier == "B"], "B_2")

  # NOHIT tier exists but has no assigned vars
  expect_false("NOHIT" %in% kn$vars$tier)
})

# ──────────────────────────────────────────────────────────────────────────────
# Helpers: is_after()
# ──────────────────────────────────────────────────────────────────────────────

test_that("is_after returns FALSE when any tier is missing", {
  kn <- knowledge() |> add_vars(c("A", "B"))
  expect_false(is_after("A", "B", kn))
})

# ──────────────────────────────────────────────────────────────────────────────
# Helpers: order_restrict_amat_cpdag()
# ──────────────────────────────────────────────────────────────────────────────

test_that("order_restrict_amat_cpdag returns input matrix when all tier ranks are NA", {
  labs <- c("V1", "V2", "V3")
  amat <- matrix(
    c(
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(labs, labs)
  )
  kn <- knowledge() |> add_vars(labs)

  out <- order_restrict_amat_cpdag(amat, kn)
  expect_equal(out, amat)
})
