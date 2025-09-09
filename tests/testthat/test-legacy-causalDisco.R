# ──────────────────────────────────────────────────────────────────────────────
# regTest / regTestEachDir
# ──────────────────────────────────────────────────────────────────────────────

test_that("regTest handles Gaussian (linear) case and finds association", {
  set.seed(123)
  n <- 400
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n, sd = 0.5)
  Y <- 0.8 * X + 0.2 * Z + stats::rnorm(n, sd = 0.5)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(data = dat, binary = setNames(rep(FALSE, 3), names(dat)), order = "t")

  p <- regTest(1, 2, 3, suff)
  expect_true(is.numeric(p) && length(p) == 1)
  expect_lt(p, 0.01)
})

test_that("regTest handles Gaussian independence", {
  set.seed(42)
  n <- 500
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n)
  Y <- 2 * Z + stats::rnorm(n)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(data = dat, binary = setNames(rep(FALSE, 3), names(dat)), order = "t")

  p <- regTest(1, 2, 3, suff)
  expect_gte(p, 0.05)
})

test_that("regTest handles binomial (logistic) response", {
  set.seed(99)
  n <- 600
  Z <- stats::rnorm(n)
  X <- 0.7 * Z + stats::rnorm(n)
  eta <- 1.2 * X - 0.4 * Z
  pr <- 1 / (1 + exp(-eta))
  Y <- stats::rbinom(n, size = 1, prob = pr)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(data = dat, binary = c(X = FALSE, Y = TRUE, Z = FALSE), order = "t")

  p <- regTest(1, 2, 3, suff)
  expect_lt(p, 0.01)
})

test_that("regTest is symmetric", {
  set.seed(7)
  n <- 300
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n)
  Y <- 0.5 * X + 0.2 * Z + stats::rnorm(n)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(data = dat, binary = c(X = FALSE, Y = FALSE, Z = FALSE), order = "t")

  p_xy <- regTest(1, 2, 3, suff)
  p_yx <- regTest(2, 1, 3, suff)

  expect_equal(p_xy, p_yx, tolerance = 1e-12)
})

test_that("regTestEachDir removes NAs", {
  set.seed(2024)
  n <- 300
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n)
  Y <- 0.8 * X + stats::rnorm(n)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  dat$X[sample.int(n, 30)] <- NA
  dat$Z[sample.int(n, 20)] <- NA

  suff <- list(data = dat, binary = c(X = FALSE, Y = FALSE, Z = FALSE), order = "t")

  p <- regTest(1, 2, 3, suff)
  expect_true(is.finite(p) && p >= 0 && p <= 1)
})

test_that("regTestEachDir wraps binary S in factor()", {
  # S contains both a binary and a numeric covariate; y is Gaussian so glm converges
  set.seed(1)
  n <- 50
  dat <- tibble::tibble(
    x_num = rnorm(n),
    y_num = 1 + 2 * x_num + rnorm(n, sd = 0.5),
    s_bin = sample(c(0L, 1L), n, replace = TRUE),
    s_num = rnorm(n)
  )

  # suffStat the function expects
  suffStat <- list(
    data = dat,
    binary = c(FALSE, FALSE, TRUE, FALSE)
  )
  names(suffStat$data) <- c("x_num", "y_num", "s_bin", "s_num")

  # x = numeric, y = numeric, S = {binary, numeric}
  pval <- regTestEachDir(
    x = 1L,
    y = 2L,
    S = c(3L, 4L),
    suffStat = suffStat
  )

  # a real p-value should come back and not error
  expect_type(pval, "double")
  expect_true(is.finite(pval))
  expect_true(pval >= 0 && pval <= 1)
})


# ──────────────────────────────────────────────────────────────────────────────
# corTest
# ──────────────────────────────────────────────────────────────────────────────

test_that("corTest matches gaussCItest", {
  skip_if_not_installed("pcalg")
  set.seed(314)
  p <- 4
  n <- 800
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
  colnames(X) <- LETTERS[1:p]
  Cmat <- stats::cor(X)
  suff <- list(C = Cmat, n = n)

  p1 <- corTest(1, 2, 3, suff)
  p2 <- pcalg::gaussCItest(1, 2, 3, suff)

  expect_equal(p1, p2, tolerance = 1e-12)
})

# ──────────────────────────────────────────────────────────────────────────────
# amat
# ──────────────────────────────────────────────────────────────────────────────

test_that("amat() extracts amat/tamat depending on class", {
  obj_cpdag <- structure(list(amat = matrix(0L, 2, 2)), class = "cpdag")
  obj_pag <- structure(list(amat = matrix(1L, 2, 2)), class = "pag")
  obj_tpdag <- structure(list(tamat = matrix(0L, 2, 2)), class = "tpdag")
  obj_tpag <- structure(list(tamat = matrix(1L, 2, 2)), class = "tpag")

  expect_equal(amat(obj_cpdag), obj_cpdag$amat)
  expect_equal(amat(obj_pag), obj_pag$amat)
  expect_equal(amat(obj_tpdag), obj_tpdag$tamat)
  expect_equal(amat(obj_tpag), obj_tpag$tamat)
})

test_that("amat() converts empty discography to 0x0 amat.cpdag", {
  dg <- new_discography(tibble::tibble(from = character(), to = character(), edge_type = character()))
  A <- amat(dg)
  expect_s3_class(A, "amat.cpdag")
  expect_equal(dim(A), c(0L, 0L))
})

test_that("amat() converts cpdag-style discography to 0/1 amat.cpdag", {
  dg <- new_discography(tibble::tibble(
    from = c("A", "C", "A", "C"),
    to = c("B", "B", "D", "D"),
    edge_type = c("-->", "-->", "---", "---")
  ))
  A <- amat(dg)

  expect_s3_class(A, "amat.cpdag")
  expect_equal(rownames(A), LETTERS[1:4])
  expect_equal(colnames(A), LETTERS[1:4])

  expect_equal(A["B", "A"], 1L) # A --> B
  expect_equal(A["B", "C"], 1L) # C --> B
  expect_equal(A["A", "D"], 1L) # A --- D
  expect_equal(A["D", "A"], 1L)
  expect_equal(A["C", "D"], 1L) # C --- D
  expect_equal(A["D", "C"], 1L)

  # everything else zero
  z <- A
  z["B", "A"] <- 0L
  z["B", "C"] <- 0L
  z["A", "D"] <- 0L
  z["D", "A"] <- 0L
  z["C", "D"] <- 0L
  z["D", "C"] <- 0L
  expect_true(all(z == 0L))
})

test_that("amat() converts pag-style discography to coded amat.pag", {
  # edges:
  # A --> B
  # C o-> A
  # C --o B
  # D o-> B
  # D --o C
  dg <- new_discography(tibble::tibble(
    from = c("A", "C", "C", "D", "D"),
    to = c("B", "A", "B", "B", "C"),
    edge_type = c("-->", "o->", "--o", "o->", "--o")
  ))
  A <- amat(dg)

  expect_s3_class(A, "amat.pag")
  expect_equal(rownames(A), LETTERS[1:4])
  expect_equal(colnames(A), LETTERS[1:4])

  # code map: 0 none | 1 circle | 2 arrow | 3 tail
  # A --> B
  expect_equal(A["B", "A"], 3L)
  expect_equal(A["A", "B"], 2L)

  # C o-> A
  expect_equal(A["A", "C"], 1L)
  expect_equal(A["C", "A"], 2L)

  # C --o B      => [B,C]=1, [C,B]=3
  expect_equal(A["B", "C"], 3L)
  expect_equal(A["C", "B"], 1L)

  # D o-> B      => [B,D]=2, [D,B]=1
  expect_equal(A["B", "D"], 1L)
  expect_equal(A["D", "B"], 2L)

  # D --o C      => [C,D]=1, [D,C]=3
  expect_equal(A["C", "D"], 3L)
  expect_equal(A["D", "C"], 1L)
})

test_that("amat() from discography infers and sorts node names alphabetically", {
  dg <- new_discography(tibble::tibble(
    from = c("b"),
    to = c("a"),
    edge_type = c("-->")
  ))
  A <- amat(dg)
  expect_equal(rownames(A), c("a", "b"))
  expect_equal(colnames(A), c("a", "b"))
  expect_equal(A["a", "b"], 1L) # b --> a encoded at [a,b]
})

test_that("round-trip cpdag discography <-> amat is identity", {
  dg <- new_discography(tibble::tibble(
    from = c("A", "A", "C", "C"),
    to = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  ))

  A <- amat(dg)
  back <- discography(A)

  expect_s3_class(A, "amat.cpdag")
  expect_equal(tibble::as_tibble(back), tibble::as_tibble(dg))
})

test_that("round-trip pag discography <-> amat is identity", {
  dg <- new_discography(tibble::tibble(
    from = c("A", "C", "C", "D", "D"),
    to = c("B", "A", "B", "B", "C"),
    edge_type = c("-->", "o->", "--o", "o->", "--o")
  ))

  A <- amat(dg)
  back <- discography(A)

  expect_s3_class(A, "amat.pag")
  expect_equal(tibble::as_tibble(back), tibble::as_tibble(dg))
})


# ──────────────────────────────────────────────────────────────────────────────
# as.graphNEL
# ──────────────────────────────────────────────────────────────────────────────

test_that("as.graphNEL drops tamat class", {
  m <- matrix(c(0, 1, 0, 0), nrow = 2, byrow = TRUE)
  rownames(m) <- colnames(m) <- c("A", "B")
  attr(m, "tamat_type") <- "pdag"
  class(m) <- c("tamat", "matrix")

  g <- as.graphNEL(m)
  expect_s4_class(g, "graphNEL")

  mat_from_g <- methods::as(g, "matrix")
  expect_equal(mat_from_g, t(unclass(m)), ignore_attr = TRUE)
})

# ──────────────────────────────────────────────────────────────────────────────
# is_pdag / is_cpdag
# ──────────────────────────────────────────────────────────────────────────────

test_that("is_pdag and is_cpdag validate graphs", {
  skip_if_not_installed("pcalg")

  # A <- B : valid PDAG; not a CPDAG (orientation not compelled)
  m <- matrix(0L, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m["B", "A"] <- 1L
  expect_true(is_pdag(m))
  expect_false(is_cpdag(m))

  # A - B : valid PDAG and valid CPDAG (undirected edge in CPDAG encoding)
  m["A", "B"] <- 1L
  expect_true(is_pdag(m))
  expect_true(is_cpdag(m))
})

# ──────────────────────────────────────────────────────────────────────────────
# graph2amat
# ──────────────────────────────────────────────────────────────────────────────

test_that("graph2amat converts to adjacency matrix", {
  skip_if_not_installed("graph")

  nodes <- c("A", "B", "C")
  g <- methods::new("graphNEL", nodes = nodes, edgemode = "directed")
  g <- graph::addEdge("A", "B", g)
  g <- graph::addEdge("B", "C", g)

  A_tf <- graph2amat(g, toFrom = TRUE, type = "pdag")
  expect_true(is.matrix(A_tf))
  expect_identical(rownames(A_tf), nodes)
  expect_identical(colnames(A_tf), nodes)
  expect_identical(attr(A_tf, "tamat_type"), "pdag")

  # to→from: row = to, col = from
  expect_equal(A_tf["B", "A"], 1) # A → B
  expect_equal(A_tf["A", "B"], 0)
  expect_equal(A_tf["C", "B"], 1) # B → C
  expect_equal(A_tf["B", "C"], 0)

  A_ft <- graph2amat(g, toFrom = FALSE, type = "pdag")
  expect_equal(A_ft, t(A_tf))
  expect_identical(attr(A_ft, "tamat_type"), "pdag")
})


# ──────────────────────────────────────────────────────────────────────────────
# maxnedges
# ──────────────────────────────────────────────────────────────────────────────

test_that("maxnedges returns correct counts for small graphs", {
  expect_equal(maxnedges(1L), 0) # by definition
  expect_equal(maxnedges(2L), 1) # 1 edge
  expect_equal(maxnedges(3L), 3) # 3 edges
  expect_equal(maxnedges(4L), 6) # 6 edges
  expect_equal(maxnedges(6L), sum(1:5)) # triangular number
})

test_that("maxnedges accepts integerish numerics (e.g., 3.0)", {
  expect_equal(maxnedges(3.0), 3)
  expect_equal(maxnedges(10.0), sum(seq_len(9L)))
})

test_that("maxnedges rejects non-positive, non-integer, and invalid inputs", {
  expect_error(maxnedges(0L), "`p` must be a single positive integer")
  expect_error(maxnedges(-2L), "`p` must be a single positive integer")
  expect_error(maxnedges(2.5), "`p` must be a single positive integer")
  expect_error(maxnedges(NA_real_), "`p` must be a single positive integer")
  expect_error(maxnedges(NaN), "`p` must be a single positive integer")
  expect_error(maxnedges(Inf), "`p` must be a single positive integer")
  expect_error(maxnedges("3"), "`p` must be a single positive integer")
  expect_error(maxnedges(c(3L, 4L)), "`p` must be a single positive integer")
})

# ──────────────────────────────────────────────────────────────────────────────
# essgraph2amat
# ──────────────────────────────────────────────────────────────────────────────

test_that("essgraph2amat builds adjacency", {
  in_edges <- list(
    integer(0),
    1L,
    c(1L, 2L)
  )
  fakeEss <- list(
    field = function(name) {
      if (name == ".in.edges") {
        return(in_edges)
      }
      stop("unexpected field")
    }
  )
  class(fakeEss) <- "EssGraph"

  A <- essgraph2amat(fakeEss, p = 3)
  expect_equal(dim(A), c(3, 3))
  expect_equal(A[2, 1], 1)
  expect_equal(A[3, 1], 1)
  expect_equal(A[3, 2], 1)
})

# ──────────────────────────────────────────────────────────────────────────────
# average_degree
# ──────────────────────────────────────────────────────────────────────────────

test_that("average_degree matches manual", {
  m <- matrix(0L, 3, 3)
  m[1, 2] <- m[2, 1] <- 1
  m[1, 3] <- m[3, 1] <- 1
  m[2, 3] <- m[3, 2] <- 1
  expect_equal(average_degree(m), 2)

  m2 <- matrix(0L, 3, 3)
  m2[1, 2] <- m2[2, 1] <- 1
  m2[2, 3] <- m2[3, 2] <- 1
  expect_equal(average_degree(m2), 4 / 3)
})

# ──────────────────────────────────────────────────────────────────────────────
# nedges
# ──────────────────────────────────────────────────────────────────────────────

test_that("nedges counts correctly", {
  m <- matrix(0L, 4, 4)
  m[lower.tri(m)] <- 1
  m <- m + t(m)
  m[m > 0] <- 1L
  expect_equal(nedges(m), 6)

  p4 <- matrix(0L, 4, 4)
  p4[1, 2] <- p4[2, 1] <- 1
  p4[2, 3] <- p4[3, 2] <- 1
  p4[3, 4] <- p4[4, 3] <- 1
  expect_equal(nedges(p4), 3)
})
