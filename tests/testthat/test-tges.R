# tests/test-tges-knowledge.R

# ──────────────────────────────────────────────────────────────────────────────
# Helper tests (pure functions; no external deps)
# ──────────────────────────────────────────────────────────────────────────────

test_that("create_adj_matrix_from_list builds the expected adjacency matrix", {
  lst <- list(
    A = c(2, 3),
    B = integer(0),
    C = 1L
  )
  names(lst) <- c("A", "B", "C")

  M <- create_adj_matrix_from_list(lst)
  expect_true(is.matrix(M))
  expect_identical(rownames(M), c("A", "B", "C"))
  expect_identical(colnames(M), c("A", "B", "C"))

  expect_identical(as.integer(M["A", ]), c(0L, 1L, 1L))
  expect_identical(as.integer(M["B", ]), c(0L, 0L, 0L))
  expect_identical(as.integer(M["C", ]), c(1L, 0L, 0L))
})

test_that("create_list_from_adj_matrix inverts create_adj_matrix_from_list", {
  original <- list(
    N1 = c(2L),
    N2 = integer(0),
    N3 = c(1L, 2L)
  )
  names(original) <- c("N1", "N2", "N3")

  M <- create_adj_matrix_from_list(original)
  back <- create_list_from_adj_matrix(M)

  expect_identical(names(back), names(original))
  expect_setequal(back$N1, original$N1)
  expect_setequal(back$N2, original$N2)
  expect_setequal(back$N3, original$N3)
})

# ──────────────────────────────────────────────────────────────────────────────
# tges() input-guard & deprecation-path tests (no external deps)
# ──────────────────────────────────────────────────────────────────────────────

test_that("tges() errors when both knowledge and order are supplied", {
  fake_score <- list(
    pp.dat = list(vertex.count = 2L),
    .nodes = c("T1_x", "T2_y")
  )
  kn <- .build_knowledge_from_order(c("T1", "T2"), vnames = fake_score$.nodes)

  expect_error(
    tges(score = fake_score, knowledge = kn, order = c("T1", "T2")),
    "Both `knowledge` and `order` supplied. Please supply a knowledge object.",
    fixed = TRUE
  )
})

test_that("tges() with legacy `order` emits a deprecation warning", {
  fake_score <- list(
    pp.dat = list(vertex.count = 2L),
    .nodes = c("T1_x", "T2_y")
  )

  expect_warning(
    try(tges(score = fake_score, order = c("T1", "T2")), silent = TRUE),
    regexp = "`order` is deprecated",
    fixed = FALSE
  )
})

test_that("tges() errors clearly if score is malformed (missing fields)", {
  bad_score <- list()
  expect_error(
    tges(score = bad_score, knowledge = knowledge()),
    regexp = "", fixed = FALSE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Knowledge-object path: real score, real search, no mocks
# ──────────────────────────────────────────────────────────────────────────────

test_that("tges(order=...) warns and returns discography", {
  set.seed(2)
  n <- 200
  child_x <- rnorm(n)
  youth_y <- child_x + rnorm(n)
  oldage_z <- youth_y + rnorm(n)
  df <- data.frame(child_x, youth_y, oldage_z)

  ctor <- getFromNamespace("GaussL0penIntScoreORDER", ns = "causalDisco")
  score <- ctor(
    data      = as.matrix(df),
    nodes     = colnames(df),
    lambda    = 0.5 * log(nrow(df)),
    format    = "raw",
    intercept = TRUE,
    order     = rep(1L, ncol(df))
  )

  expect_warning(
    g <- tges(score = score, order = c("child", "youth", "oldage")),
    regexp = "deprecated", fixed = FALSE
  )
  expect_s3_class(g, "discography")
})

test_that("tges() errors when both knowledge and order are supplied (full path)", {
  set.seed(3)
  n <- 100
  df <- data.frame(child_x = rnorm(n), youth_y = rnorm(n), oldage_z = rnorm(n))

  ctor <- getFromNamespace("GaussL0penIntScoreORDER", ns = "causalDisco")
  score <- ctor(
    data      = as.matrix(df),
    nodes     = colnames(df),
    lambda    = 0.5 * log(nrow(df)),
    format    = "raw",
    intercept = TRUE,
    order     = rep(1L, ncol(df))
  )

  kn <- {
    kn0 <- knowledge() |> add_vars(colnames(df))
    kn1 <- add_tier(kn0, !!"child")
    kn2 <- add_tier(kn1, !!"youth", after = "child")
    add_tier(kn2, !!"oldage", after = "youth")
  }
  idx <- match(colnames(df), kn$vars$var)
  kn$vars$tier[idx[grepl("^child", colnames(df))]] <- "child"
  kn$vars$tier[idx[grepl("^youth", colnames(df))]] <- "youth"
  kn$vars$tier[idx[grepl("^oldage", colnames(df))]] <- "oldage"

  expect_error(
    tges(score = score, knowledge = kn, order = c("child", "youth", "oldage")),
    "Both `knowledge` and `order` supplied",
    fixed = FALSE
  )
})

test_that("tges() errors clearly if score is malformed (suppress incidental warnings)", {
  bad_score <- list()
  expect_error(suppressWarnings(tges(score = bad_score, knowledge = knowledge())))
})

test_that("tges() respects temporal knowledge by removing forbidden later->earlier parents", {
  set.seed(5)
  n <- 500
  child_x <- rnorm(n)
  youth_y <- 0.9 * child_x + rnorm(n, sd = 0.6)
  oldage_z <- 0.9 * youth_y + rnorm(n, sd = 0.6)
  child_x <- 1.5 * oldage_z + rnorm(n, sd = 0.4)

  df <- data.frame(
    child_x  = as.numeric(scale(child_x)),
    youth_y  = as.numeric(scale(youth_y)),
    oldage_z = as.numeric(scale(oldage_z))
  )

  ctor <- getFromNamespace("GaussL0penIntScoreORDER", ns = "causalDisco")
  score <- ctor(
    data      = as.matrix(df),
    nodes     = colnames(df),
    lambda    = 0.1,
    format    = "raw",
    intercept = TRUE,
    order     = rep(1L, ncol(df))
  )

  kn <- {
    kn0 <- knowledge() |> add_vars(colnames(df))
    kn1 <- add_tier(kn0, !!"child")
    kn2 <- add_tier(kn1, !!"youth", after = "child")
    add_tier(kn2, !!"oldage", after = "youth")
  }
  idx <- match(colnames(df), kn$vars$var)
  kn$vars$tier[idx[grepl("^child", colnames(df))]] <- "child"
  kn$vars$tier[idx[grepl("^youth", colnames(df))]] <- "youth"
  kn$vars$tier[idx[grepl("^oldage", colnames(df))]] <- "oldage"

  g <- tges(score = score, knowledge = kn, verbose = FALSE)
  expect_s3_class(g, "discography")

  ti <- setNames(
    match(kn$vars$tier[match(names(df), kn$vars$var)], kn$tiers$label),
    names(df)
  )
  directed <- subset(g, edge_type == "-->")
  if (nrow(directed) > 0) {
    ok <- ti[directed$from] <= ti[directed$to]
    expect_true(all(ok))
  } else {
    succeed("No directed edges; knowledge prevented invalid parents.")
  }
})

test_that("tges() emits the clear malformed score error", {
  bad_score <- list() # missing pp.dat / .nodes
  expect_error(
    tges(score = bad_score, knowledge = knowledge()),
    "Invalid `score` object supplied: must have `score\\$pp\\.dat\\$vertex\\.count` \\(scalar integer\\) and `\\.nodes`\\.",
    fixed = FALSE
  )
})

test_that("to_adj_mat handles NULL, matrix, graphNEL, and pcAlgo-like @graph (no mocks)", {
  # NULL -> NULL
  expect_null(to_adj_mat(NULL))

  # matrix -> identity
  M <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3, byrow = TRUE)
  expect_identical(to_adj_mat(M), M)

  # graphNEL -> matrix
  skip_if_not_installed("graph")
  gnel <- graph::graphNEL(nodes = c("A", "B", "C"), edgemode = "directed")
  # add A->B, B->C
  gnel <- graph::addEdge("A", "B", gnel)
  gnel <- graph::addEdge("B", "C", gnel)
  amat_from_gnel <- to_adj_mat(gnel)
  expect_true(is.matrix(amat_from_gnel))
  expect_identical(dim(amat_from_gnel), c(3L, 3L))
  expect_identical(rownames(amat_from_gnel), c("A", "B", "C"))
  expect_identical(colnames(amat_from_gnel), c("A", "B", "C"))
  expect_equal(amat_from_gnel["A", "B"], 1)
  expect_equal(amat_from_gnel["B", "C"], 1)

  # pcAlgo-like S4 with @graph slot -> matrix
  methods::setClass("pcAlgoLike", slots = list(graph = "graphNEL"))
  pal <- methods::new("pcAlgoLike", graph = gnel)
  amat_from_pal <- to_adj_mat(pal)
  # expect_true(is.matrix(amat_from_pal))
  expect_identical(amat_from_pal, amat_from_gnel)
})

test_that("GaussL0penIntScoreORDER$local.score returns -Inf when order is violated", {
  set.seed(101)
  X <- cbind(x = rnorm(50), y = rnorm(50))
  colnames(X) <- c("x", "y")
  ctor <- getFromNamespace("GaussL0penIntScoreORDER", ns = "causalDisco")

  sc <- ctor(
    data      = X,
    nodes     = colnames(X),
    lambda    = 0.5 * log(nrow(X)),
    format    = "raw",
    intercept = TRUE,
    use.cpp   = FALSE, # ensure c.fcn == "none" path
    order     = c(2, 1) # x has later order than y; y cannot have parent x
  )

  # ask for local score of y with forbidden parent x -> -Inf
  val <- sc$local.score(vertex = 2L, parents = 1L)
  expect_identical(val, -Inf)
})

test_that("GaussL0penIntScoreORDER$local.score covers raw and scatter branches", {
  set.seed(202)
  n <- 80
  x <- rnorm(n)
  y <- 0.8 * x + rnorm(n, sd = 0.5)
  X <- cbind(x = x, y = y)

  ctor <- getFromNamespace("GaussL0penIntScoreORDER", ns = "causalDisco")

  # raw branch
  sc_raw <- ctor(
    data      = X,
    nodes     = colnames(X),
    lambda    = 0.5 * log(nrow(X)),
    format    = "raw",
    intercept = TRUE,
    use.cpp   = FALSE,
    order     = c(1, 1)
  )
  s1 <- sc_raw$local.score(vertex = 2L, parents = 1L)
  expect_true(is.finite(s1))

  # scatter branch: build scatter matrices as expected by the score class
  # mimic internal pre-processing: add intercept's scatter row/col
  Z <- cbind(1, X) # intercept + data
  S <- crossprod(Z) # (p+1) x (p+1) scatter
  sc_scatter <- ctor(
    data      = X,
    nodes     = colnames(X),
    lambda    = 0.5 * log(nrow(X)),
    format    = "scatter",
    intercept = TRUE,
    use.cpp   = FALSE,
    order     = c(1, 1)
  )
  # populate the minimal fields the R-path reads when .format == "scatter"
  sc_scatter$pp.dat$vertex.count <- ncol(X)
  sc_scatter$pp.dat$intercept <- TRUE
  sc_scatter$pp.dat$scatter <- list(S)
  sc_scatter$pp.dat$scatter.index <- c(1L, 1L)
  sc_scatter$pp.dat$data.count <- rep(n, ncol(X))

  s2 <- sc_scatter$local.score(vertex = 2L, parents = 1L)
  expect_true(is.finite(s2))
  # both branches should give comparable (not necessarily equal) finite values
  expect_true(is.finite(s1) && is.finite(s2))
})
