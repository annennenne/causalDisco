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
# tpc_run()
# ──────────────────────────────────────────────────────────────────────────────

test_that("tpc_run returns tpdag on example data", {
  set.seed(123)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  res <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.01,
    test = regTest,
    output = "tpdag"
  )

  expect_s3_class(res, "tpdag")
  A <- res$tamat
  expect_true(is.matrix(A))
  expect_identical(rownames(A), colnames(A))
  expect_setequal(rownames(A), names(tpcExample))
  expect_gt(res$ntests, 0)
  expect_identical(res$psi, 0.01)
})

test_that("tpc_run returns tskeleton on example data", {
  set.seed(321)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  res <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.02,
    test = regTest,
    output = "tskeleton"
  )

  expect_s3_class(res, "tskeleton")
  A <- res$tamat
  expect_true(is.matrix(A))
  expect_identical(rownames(A), colnames(A))
  expect_setequal(rownames(A), names(tpcExample))
})

test_that("tpc_run returns pcAlgo output", {
  set.seed(42)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  res <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.05,
    test = regTest,
    output = "pcAlgo"
  )

  A <- graph2amat(res, toFrom = FALSE)
  expect_true(is.matrix(A))
  expect_identical(rownames(A), colnames(A))
  expect_setequal(rownames(A), names(tpcExample))
})

test_that("tpc_run works with corTest", {
  set.seed(777)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  res <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.01,
    test = corTest,
    output = "tpdag"
  )

  expect_s3_class(res, "tpdag")
  expect_gt(res$ntests, 0)
})

test_that("tpc_run respects forbidden knowledge", {
  set.seed(999)
  data(tpcExample, package = "causalDisco")

  kn <- build_kn_from_order()

  vars <- names(tpcExample)
  x <- vars[1]
  y <- vars[2]
  kn_forb <- kn |> forbid_edge(!!as.name(x) ~ !!as.name(y))

  res <- tpc_run(
    data = tpcExample,
    knowledge = kn_forb,
    alpha = 0.02,
    test = regTest,
    output = "tpdag"
  )

  A <- res$tamat
  # directed-as-undirected constraints: both directions should be absent
  expect_identical(A[rownames(A) == x, colnames(A) == y], 0)
  expect_identical(A[rownames(A) == y, colnames(A) == x], 0)
})

test_that("tpc_run(order=...) runs and returns tpdag, throws deprecation warning", {
  set.seed(202)
  data(tpcExample, package = "causalDisco")

  ord <- c("child", "youth", "oldage")

  expect_warning(
    res <- tpc_run(
      data = tpcExample,
      order = ord,
      alpha = 0.01,
      test = regTest,
      output = "tpdag"
    )
  )
  expect_s3_class(res, "tpdag")
  A <- res$tamat
  expect_true(is.matrix(A))
  expect_identical(rownames(A), colnames(A))
})

test_that("tpc_run supports tskeleton, pcAlgo, and knowledgeable_caugi outputs", {
  set.seed(707)
  data(tpcExample, package = "causalDisco")

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ tidyselect::starts_with("child"),
      youth ~ tidyselect::starts_with("youth"),
      oldage ~ tidyselect::starts_with("oldage")
    )
  )

  res_skel <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.03,
    test = regTest,
    output = "tskeleton"
  )
  expect_s3_class(res_skel, "tskeleton")

  res_pc <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.03,
    test = regTest,
    output = "pcAlgo"
  )
  A_pc <- graph2amat(res_pc, toFrom = FALSE)
  expect_true(is.matrix(A_pc))

  res_disco <- tpc_run(
    data = tpcExample,
    knowledge = kn,
    alpha = 0.03,
    test = regTest,
    output = "caugi"
  )
  expect_s3_class(res_disco, "knowledgeable_caugi")
})

# ──────────────────────────────────────────────────────────────────────────────
# tpc_run() guards and errors
# ──────────────────────────────────────────────────────────────────────────────


test_that("tpc_run errors when both knowledge and order are supplied", {
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
    tpc_run(
      data = tpcExample,
      knowledge = kn,
      order = ord,
      alpha = 0.015,
      test = regTest,
      output = "tpdag"
    ),
    "Both `knowledge` and `order` supplied. Please supply a knowledge object.",
    fixed = TRUE
  )
})

test_that("tpc_run input guards fail fast with clear messages", {
  df <- data.frame(a = 1:3, b = 1:3)
  kn <- knowledge() |> add_vars(names(df))

  expect_error(
    tpc_run(data = df, knowledge = kn, output = "nope"),
    "Output must be tpdag, tskeleton, pcAlgo, or caugi",
    fixed = TRUE
  )
  expect_error(
    tpc_run(data = df, knowledge = kn, methodNA = "oops"),
    "Invalid choice of method for handling NA values.",
    fixed = TRUE
  )
  expect_error(
    tpc_run(data = NULL, suffStat = NULL, knowledge = knowledge()),
    "Either data or sufficient statistic must be supplied.",
    fixed = TRUE
  )
})

test_that("tpc_run NA handling: error on NAs with methodNA = 'none', cc with zero rows", {
  df1 <- data.frame(a = c(1, NA), b = c(2, NA))
  kn1 <- knowledge() |> add_vars(names(df1))

  expect_error(
    tpc_run(data = df1, knowledge = kn1, methodNA = "none"),
    "Inputted data contain NA values, but no method for handling missing NAs was supplied.",
    fixed = TRUE
  )

  df2 <- data.frame(a = c(NA, NA), b = c(NA, NA))
  kn2 <- knowledge() |> add_vars(names(df2))

  expect_error(
    tpc_run(data = df2, knowledge = kn2, methodNA = "cc"),
    "contain no complete cases.",
    fixed = TRUE
  )
})

test_that("tpc_run errors when varnames are unknown with suffStat-only usage", {
  suff <- list(dummy = TRUE)
  expect_error(
    tpc_run(data = NULL, suffStat = suff, knowledge = knowledge(), varnames = NULL),
    "Could not determine variable names. Supply `data` or `varnames`.",
    fixed = TRUE
  )
})

test_that("tpc_run demands suffStat for non-builtin test functions", {
  set.seed(1)
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  kn <- knowledge() |> add_vars(names(df))
  strange_test <- function(x, y, S, suffStat) 0

  expect_error(
    tpc_run(data = df, knowledge = kn, test = strange_test),
    "suffStat needs to be supplied when using a non-builtin test.",
    fixed = TRUE
  )
})


test_that("tpc_run adds missing vars to knowledge and uses provided suffStat (tskeleton path)", {
  set.seed(11)
  df <- data.frame(
    child_x = rnorm(40),
    youth_y = rnorm(40),
    oldage_z = rnorm(40)
  )

  kn0 <- knowledge() |> add_vars(c("child_x", "youth_y")) # missing oldage_z
  suff <- make_suff_stat(df, type = "regTest")

  res <- tpc_run(
    data = NULL,
    knowledge = kn0,
    alpha = 0.1,
    test = regTest,
    suffStat = suff,
    output = "tskeleton",
    varnames = names(df)
  )

  expect_s3_class(res, "tskeleton")
  A <- res$tamat
  expect_true(is.matrix(A))
  expect_identical(rownames(A), colnames(A))
  expect_setequal(rownames(A), names(df))

  kn_bad <- knowledge() |> add_vars(c("child_a")) # missing oldage_z

  expect_error(
    tpc_run(
      data = NULL,
      knowledge = kn_bad,
      alpha = 0.1,
      test = regTest,
      suffStat = suff,
      output = "tskeleton",
      varnames = names(df)
    ),
    "Knowledge contains variables not present in `data`"
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Helpers: make_suff_stat()
# ──────────────────────────────────────────────────────────────────────────────


test_that("make_suff_stat() returns correct suffStat for different tests and fails correctly", {
  set.seed(12)
  df <- data.frame(
    child_x = rnorm(40),
    youth_y = rnorm(40),
    oldage_z = rnorm(40)
  )
  suff <- make_suff_stat(df, type = "regTest")
  expect_true(is.list(suff))
  expect_true(!is.null(suff$data))
  expect_true(!is.null(suff$bin))

  suff2 <- make_suff_stat(df, type = "corTest")
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
    .build_knowledge_from_order(order = c("T1", "T2"), data = NULL, vnames = NULL),
    "`data` is NULL, so `vnames` should be provided.",
    fixed = TRUE
  )
})

test_that(".build_knowledge_from_order builds tiers in declared order (vnames path)", {
  vnames <- c("T1_x", "T1_y", "T2_a", "zzz")
  kn <- .build_knowledge_from_order(order = c("T1", "T2"), data = NULL, vnames = vnames)

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
  kn <- .build_knowledge_from_order(order = c("x", "x1"), data = NULL, vnames = vnames)

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
  kn <- .build_knowledge_from_order(order = c("A", "NOHIT", "B"), data = NULL, vnames = vnames)

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
      0, 1, 0,
      0, 0, 1,
      1, 0, 0
    ),
    nrow = 3, byrow = TRUE, dimnames = list(labs, labs)
  )
  kn <- knowledge() |> add_vars(labs)

  out <- order_restrict_amat_cpdag(amat, kn)
  expect_equal(out, amat)
})
