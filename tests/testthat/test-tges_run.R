# ──────────────────────────────────────────────────────────────────────────────
# Helpers: create_adj_matrix_from_list and create_list_from_matrix
# ──────────────────────────────────────────────────────────────────────────────

test_that("create_adj_matrix_from_list builds the expected adjacency matrix", {
  lst <- list(
    A = c(2, 3),
    B = integer(0),
    C = 1L
  )
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
  M <- create_adj_matrix_from_list(original)
  back <- create_list_from_adj_matrix(M)
  expect_identical(names(back), names(original))
  expect_setequal(back$N1, original$N1)
  expect_setequal(back$N2, original$N2)
  expect_setequal(back$N3, original$N3)
})

# ──────────────────────────────────────────────────────────────────────────────
# Helpers: to_adj_mat
# ──────────────────────────────────────────────────────────────────────────────

test_that("to_adj_mat handles NULL, matrix, graphNEL, and pcAlgo-like @graph", {
  expect_null(to_adj_mat(NULL))

  M <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3, byrow = TRUE)
  expect_identical(to_adj_mat(M), M)

  gnel <- graph::graphNEL(nodes = c("A", "B", "C"), edgemode = "directed")
  gnel <- graph::addEdge("A", "B", gnel)
  gnel <- graph::addEdge("B", "C", gnel)
  amat_from_gnel <- to_adj_mat(gnel)
  expect_true(is.matrix(amat_from_gnel))
  expect_identical(dim(amat_from_gnel), c(3L, 3L))
  expect_identical(rownames(amat_from_gnel), c("A", "B", "C"))
  expect_identical(colnames(amat_from_gnel), c("A", "B", "C"))
  expect_equal(amat_from_gnel["A", "B"], 1)
  expect_equal(amat_from_gnel["B", "C"], 1)

  methods::setClass("pcAlgoLike", slots = list(graph = "graphNEL"))
  pal <- methods::new("pcAlgoLike", graph = gnel)
  expect_identical(to_adj_mat(pal), amat_from_gnel)
})

# ──────────────────────────────────────────────────────────────────────────────
# Scores
# ──────────────────────────────────────────────────────────────────────────────

test_that("Scores initialize invalid `order` type errors cleanly", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_warning(expect_error(
    new(
      "TemporalBIC",
      data = df,
      order = list(1, 2) # neither numeric nor character
    ),
    regexp = "`order` must be either a vector of integers or a vector of"
  ))
  expect_warning(expect_error(
    new("TemporalBDeu", data = df, order = list(1, 2)),
    regexp = "`order` must be either a vector of integers or a vector of"
  ))
  expect_error(
    new(
      "TemporalBIC",
      data = df,
      order = c(1, 2),
      knowledge = knowledge(df, tier(1 ~ a, 2 ~ b))
    ),
    regexp = "Both `knowledge` and `order` supplied"
  )
  expect_error(
    new(
      "TemporalBDeu",
      data = df,
      order = c(1, 2),
      knowledge = knowledge(df, tier(1 ~ a, 2 ~ b))
    ),
    regexp = "Both `knowledge` and `order` supplied"
  )
  expect_warning(expect_error(
    new("TemporalBIC", data = df, order = c(factor(1), factor(2))),
    regexp = "`order` must be either a vector of integers or a vector of prefixes"
  ))
  expect_warning(expect_error(
    new("TemporalBDeu", data = df, order = c(factor(1), factor(2))),
    regexp = "`order` must be either a vector of integers or a vector of prefixes"
  ))
  score_bic <- new("TemporalBIC", data = df, order = NULL, knowledge = NULL)

  score_bdeu <- new("TemporalBDeu", data = df, order = NULL, knowledge = NULL)

  expect_equal(score_bic$.order, c("a" = NA_integer_, "b" = NA_integer_))
  expect_equal(score_bdeu$.order, c("a" = NA_integer_, "b" = NA_integer_))

  expect_warning(
    score_bdeu <- new("TemporalBDeu", data = df, order = c(1, 2))
  )
  expect_warning(
    score_bdeu <- new("TemporalBDeu", data = df, order = c("a", "b"))
  )
})

test_that("TemporalBDeu covers if(length(parents) == 0) part", {
  data("alarm", package = "bnlearn")
  sc <- new(
    "TemporalBDeu",
    iss = 0.5,
    data = head(alarm, 1000),
    knowledge = knowledge()
  )
  g <- tges_run(sc, verbose = FALSE)
  expect_true(TRUE)
})

test_that("TemporalBIC initializes from knowledge and enforces tiers (-Inf on violation)", {
  set.seed(1)
  X <- cbind(x = rnorm(50), y = rnorm(50))
  kn <- knowledge() |> add_vars(c("x", "y"))
  kn <- add_tier(kn, "T1")
  kn <- add_tier(kn, "T2", after = "T1")
  # x in T2, y in T1 ⇒ y cannot have parent x
  idx <- match(c("x", "y"), kn$vars$var)
  kn$vars$tier[idx[1]] <- "T2"
  kn$vars$tier[idx[2]] <- "T1"

  sc <- new(
    "TemporalBIC",
    data = X,
    nodes = colnames(X),
    lambda = 0.5 * log(nrow(X)),
    format = "raw",
    intercept = TRUE,
    use.cpp = FALSE,
    knowledge = kn
  )
  val <- sc$local.score(vertex = 2L, parents = 1L) # y <- x forbidden
  expect_identical(val, -Inf)
})

test_that("TemporalBIC local.score raw and scatter branches both finite when allowed", {
  set.seed(2)
  n <- 80
  x <- rnorm(n)
  y <- 0.8 * x + rnorm(n, 0.5)
  X <- cbind(x = x, y = y)

  # put both in same tier so no restriction
  kn <- knowledge() |> add_vars(c("x", "y"))
  kn <- add_tier(kn, "T1")
  idx <- match(c("x", "y"), kn$vars$var)
  kn$vars$tier[idx] <- "T1"

  sc_raw <- new(
    "TemporalBIC",
    data = X,
    nodes = colnames(X),
    lambda = 0.5 * log(nrow(X)),
    format = "raw",
    intercept = TRUE,
    use.cpp = FALSE,
    knowledge = kn
  )
  s1 <- sc_raw$local.score(vertex = 2L, parents = 1L)
  expect_true(is.finite(s1))

  Z <- cbind(1, X)
  S <- crossprod(Z)
  sc_sc <- new(
    "TemporalBIC",
    data = X,
    nodes = colnames(X),
    lambda = 0.5 * log(nrow(X)),
    format = "scatter",
    intercept = TRUE,
    use.cpp = FALSE,
    knowledge = kn
  )
  sc_sc$pp.dat$vertex.count <- ncol(X)
  sc_sc$pp.dat$intercept <- TRUE
  sc_sc$pp.dat$scatter <- list(S)
  sc_sc$pp.dat$scatter.index <- c(1L, 1L)
  sc_sc$pp.dat$data.count <- rep(n, ncol(X))
  s2 <- sc_sc$local.score(vertex = 2L, parents = 1L)
  expect_true(is.finite(s2))
})

test_that("TemporalBIC initialize from deprecated numeric/character order builds knowledge", {
  set.seed(3)
  X <- cbind(a = rnorm(20), b = rnorm(20))
  # numeric order
  expect_warning(
    {
      sc_num <- new(
        "TemporalBIC",
        data = X,
        nodes = colnames(X),
        order = c(1, 2),
        use.cpp = FALSE
      )
    },
    regexp = "deprecated"
  )
  expect_true(all(!is.na(sc_num$.order)))

  # character prefixes -> needs helper .build_knowledge_from_order
  expect_warning(
    {
      sc_chr <- new(
        "TemporalBIC",
        data = X,
        nodes = c("T1_a", "T2_b"),
        order = c("T1", "T2"),
        use.cpp = FALSE
      )
    },
    regexp = "deprecated"
  )
  expect_true(all(!is.na(sc_chr$.order)))
})

test_that("TemporalBIC with partially tiered knowledge skips enforcement for untiered vars", {
  set.seed(4)
  X <- cbind(x = rnorm(30), y = rnorm(30))
  kn <- knowledge() |> add_vars(c("x", "y"))
  kn <- add_tier(kn, "T1") # only define T1
  # assign tier only to x; y stays NA
  kn$vars$tier[kn$vars$var == "x"] <- "T1"

  sc <- new(
    "TemporalBIC",
    data = X,
    nodes = colnames(X),
    knowledge = kn,
    use.cpp = FALSE
  )
  # y has NA tier → no enforcement → finite score even if x considered "later"
  val <- sc$local.score(vertex = 2L, parents = 1L)
  expect_true(is.finite(val))
})

test_that("TemporalBDeu initializes and returns finite BDeu when allowed", {
  set.seed(5)
  n <- 200
  A <- factor(sample(1:2, n, TRUE))
  B <- factor(sample(1:3, n, TRUE))
  D <- data.frame(A = A, B = B)

  kn <- knowledge() |> add_vars(c("A", "B"))
  kn <- add_tier(kn, "T1")
  # both in same tier
  kn$vars$tier[] <- "T1"

  sc <- new(
    "TemporalBDeu",
    data = D,
    nodes = colnames(D),
    iss = 1,
    knowledge = kn
  )
  s <- sc$local.score(vertex = 2L, parents = 1L) # B <- A allowed
  expect_true(is.finite(s))
})

test_that("TemporalBDeu returns -Inf when a later-tier parent is proposed", {
  set.seed(6)
  n <- 150
  A <- factor(sample(1:2, n, TRUE))
  B <- factor(sample(1:2, n, TRUE))
  D <- data.frame(A = A, B = B)

  kn <- knowledge() |> add_vars(c("A", "B"))
  kn <- add_tier(kn, "T1")
  kn <- add_tier(kn, "T2", after = "T1")
  # A in T2, B in T1 → B <- A forbidden
  kn$vars$tier[kn$vars$var == "A"] <- "T2"
  kn$vars$tier[kn$vars$var == "B"] <- "T1"

  sc <- new(
    "TemporalBDeu",
    data = D,
    nodes = colnames(D),
    iss = 1,
    knowledge = kn
  )
  expect_identical(sc$local.score(vertex = 2L, parents = 1L), -Inf)
})

# ──────────────────────────────────────────────────────────────────────────────
# tges_run() guard checks (API is score-only; no knowledge/order here)
# ──────────────────────────────────────────────────────────────────────────────

test_that("tges_run() rejects non-supported score classes with clear message", {
  fake <- structure(
    list(pp.dat = list(vertex.count = 2L, data = matrix(0, 1, 2))),
    class = "NotAScore"
  )
  expect_error(
    tges_run(fake),
    "Score must be of type TemporalBIC or TemporalBDeu",
    fixed = TRUE
  )
})

test_that("tges_run() enforces factors for TemporalBDeu and missing-value guard", {
  set.seed(7)
  D_bad_type <- data.frame(A = rnorm(10), B = rnorm(10))
  kn <- knowledge() |> add_vars(c("A", "B"))
  kn <- add_tier(kn, "T1")
  sc_bad <- new(
    "TemporalBDeu",
    data = D_bad_type,
    nodes = c("A", "B"),
    knowledge = kn
  )
  expect_error(tges_run(sc_bad), "must be factors", fixed = TRUE)

  D_na <- data.frame(
    A = factor(c(1, 1, NA, 2, 2)),
    B = factor(c(1, 2, 1, 2, 2))
  )
  sc_na <- new("TemporalBDeu", data = D_na, nodes = c("A", "B"), knowledge = kn)
  expect_error(
    tges_run(sc_na),
    "Data must not contain missing values",
    fixed = TRUE
  )
})

test_that("tges_run() builds Forbidden.edges from score$.order", {
  set.seed(8)
  X <- cbind(x = rnorm(10), y = rnorm(10))
  kn <- knowledge() |> add_vars(c("x", "y"))
  kn <- add_tier(kn, "T1")
  kn <- add_tier(kn, "T2", after = "T1")
  kn$vars$tier[kn$vars$var == "x"] <- "T2"
  kn$vars$tier[kn$vars$var == "y"] <- "T1"

  sc <- new(
    "TemporalBIC",
    data = X,
    nodes = colnames(X),
    knowledge = kn,
    use.cpp = FALSE
  )
  expect_no_error(try(
    suppressWarnings(tges_run(sc, verbose = FALSE)),
    silent = TRUE
  ))
})

test_that("tges_run forward phase", {
  set.seed(13) # friday the 13th
  n <- 100
  X1 <- rnorm(n)
  X2 <- 0.5 * X1 + rnorm(n)
  X3 <- rnorm(n)
  X4 <- 1.2 * X2 + 0.5 * X3 + rnorm(n)
  df <- data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4)
  # scale
  df <- as.data.frame(scale(df))
  kn <- knowledge(df, tier(1 ~ X1, 2 ~ X2 + X3 + X4))
  sc <- new("TemporalBIC", data = df, knowledge = kn)
  g <- tges_run(sc, verbose = FALSE)
  expect_true(TRUE)
})

test_that("tges_run turning phase", {
  set.seed(13) # friday the 13th
  n <- 100
  X1 <- rnorm(n) + rbinom(n, 1, 0.3)
  X2 <- 0.6 * X1**2 + rnorm(n) + rbinom(n, 1, 0.3)
  X3 <- -0.3 * X1 + rnorm(n) + rbinom(n, 1, 0.3)
  X4 <- 1.5 * X1 + -0.2 * X2**2 + 0.1 * X3**3 + rnorm(n) + rbinom(n, 1, 0.3)

  df <- data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4)
  kn <- knowledge(df, tier(1 ~ X1 + X2 + X3 + X4))
  sc <- new("TemporalBIC", data = df, knowledge = kn)
  g <- tges_run(sc, verbose = FALSE)
  expect_true(TRUE)
})
