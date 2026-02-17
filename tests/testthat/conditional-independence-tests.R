# ──────────────────────────────────────────────────────────────────────────────
# reg_test / reg_test_each_dir
# ──────────────────────────────────────────────────────────────────────────────

test_that("reg_test handles Gaussian (linear) case and finds association", {
  set.seed(1405)
  n <- 400
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n, sd = 0.5)
  Y <- 0.8 * X + 0.2 * Z + stats::rnorm(n, sd = 0.5)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(
    data = dat,
    binary = setNames(rep(FALSE, 3), names(dat)),
    order = "t"
  )

  p <- reg_test(1, 2, 3, suff)
  expect_true(is.numeric(p) && length(p) == 1)
  expect_lt(p, 0.01)
})

test_that("reg_test handles Gaussian independence", {
  set.seed(1405)
  n <- 500
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n)
  Y <- 2 * Z + stats::rnorm(n)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(
    data = dat,
    binary = setNames(rep(FALSE, 3), names(dat)),
    order = "t"
  )

  p <- reg_test(1, 2, 3, suff)
  expect_gte(p, 0.05)
})

test_that("reg_test handles binomial (logistic) response", {
  set.seed(1405)
  n <- 600
  Z <- stats::rnorm(n)
  X <- 0.7 * Z + stats::rnorm(n)
  eta <- 1.2 * X - 0.4 * Z
  pr <- 1 / (1 + exp(-eta))
  Y <- stats::rbinom(n, size = 1, prob = pr)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(
    data = dat,
    binary = c(X = FALSE, Y = TRUE, Z = FALSE),
    order = "t"
  )

  p <- reg_test(1, 2, 3, suff)
  expect_lt(p, 0.01)
})

test_that("reg_test is symmetric", {
  set.seed(1405)
  n <- 300
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n)
  Y <- 0.5 * X + 0.2 * Z + stats::rnorm(n)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  suff <- list(
    data = dat,
    binary = c(X = FALSE, Y = FALSE, Z = FALSE),
    order = "t"
  )

  p_xy <- reg_test(1, 2, 3, suff)
  p_yx <- reg_test(2, 1, 3, suff)

  expect_equal(p_xy, p_yx, tolerance = 1e-12)
})

test_that("reg_test_each_dir removes NAs", {
  set.seed(1405)
  n <- 300
  Z <- stats::rnorm(n)
  X <- Z + stats::rnorm(n)
  Y <- 0.8 * X + stats::rnorm(n)

  dat <- data.frame(X = X, Y = Y, Z = Z)
  dat$X[sample.int(n, 30)] <- NA
  dat$Z[sample.int(n, 20)] <- NA

  suff <- list(
    data = dat,
    binary = c(X = FALSE, Y = FALSE, Z = FALSE),
    order = "t"
  )

  p <- reg_test(1, 2, 3, suff)
  expect_true(is.finite(p) && p >= 0 && p <= 1)
})

test_that("reg_test_each_dir wraps binary S in factor()", {
  # S contains both a binary and a numeric covariate; y is Gaussian so glm converges
  set.seed(1405)
  n <- 50
  dat <- tibble::tibble(
    x_num = rnorm(n),
    y_num = 1 + 2 * x_num + rnorm(n, sd = 0.5),
    s_bin = sample(c(0L, 1L), n, replace = TRUE),
    s_num = rnorm(n)
  )

  # suff_stat the function expects
  suff_stat <- list(
    data = dat,
    binary = c(FALSE, FALSE, TRUE, FALSE)
  )
  names(suff_stat$data) <- c("x_num", "y_num", "s_bin", "s_num")

  # x = numeric, y = numeric, S = {binary, numeric}
  pval <- reg_test_each_dir(
    x = 1L,
    y = 2L,
    S = c(3L, 4L),
    suffStat = suff_stat
  )

  # a real p-value should come back and not error
  expect_type(pval, "double")
  expect_true(is.finite(pval))
  expect_true(pval >= 0 && pval <= 1)
})


# ──────────────────────────────────────────────────────────────────────────────
# cor_test
# ──────────────────────────────────────────────────────────────────────────────

test_that("cor_test matches gaussCItest", {
  set.seed(1405)
  p <- 4
  n <- 800

  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- LETTERS[1:p]

  Cmat <- stats::cor(X)
  suff <- list(C = Cmat, n = n)

  p1 <- cor_test(1, 2, 3, suff)
  p2 <- pcalg::gaussCItest(1, 2, 3, suff)

  expect_equal(p1, p2, tolerance = 1e-12)
})
