test_that("custom test works", {
  my_test <- function(x, y, conditioning_set, suff_stat) {
    C <- suff_stat$C
    n <- suff_stat$n

    vars <- c(x, y, conditioning_set)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_suff_stat <- function(data) {
    list(
      C = cor(data),
      n = nrow(data)
    )
  }

  data(tpc_example)

  my_tpc <- tpc(
    engine = "causalDisco",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat
  )
  result_tpc <- disco(data = tpc_example, method = my_tpc)
  expect_equal(class(result_tpc), "Disco")

  my_pc <- pc(
    engine = "pcalg",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat
  )
  result_pc <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result_pc), "Disco")

  # Verify alpha is respected
  my_tpc_new <- tpc(
    engine = "causalDisco",
    test = my_test,
    alpha = 0.9,
    suff_stat_fun = my_suff_stat
  )
  result_new_tpc <- disco(data = tpc_example, method = my_tpc_new)
  expect_true(nrow(result_new_tpc$caugi@edges) > nrow(result_tpc$caugi@edges))

  my_pc_new <- pc(
    engine = "pcalg",
    test = my_test,
    alpha = 0.8,
    suff_stat_fun = my_suff_stat
  )
  result_pc_new <- disco(data = tpc_example, method = my_pc_new)
  expect_true(nrow(result_pc_new$caugi@edges) > nrow(result_pc$caugi@edges))

  # Bnlearn tests

  my_test_bnlearn <- function(x, y, conditioning_set, suff_stat) {
    C <- cor(suff_stat)
    n <- nrow(suff_stat)

    vars <- c(x, y, conditioning_set)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_pc <- pc(
    engine = "bnlearn",
    test = my_test_bnlearn,
    alpha = 0.05
  )
  result_pc <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result_pc), "Disco")

  # Verify alpha is respected
  my_pc_new <- pc(
    engine = "bnlearn",
    test = my_test_bnlearn,
    alpha = 0.8
  )

  result_pc_new <- suppressWarnings(disco(
    data = tpc_example,
    method = my_pc_new
  ))
  expect_true(nrow(result_pc_new$caugi@edges) > nrow(result_pc$caugi@edges))

  # Use different naming
  my_test_bnlearn <- function(x, y, z, data) {
    C <- cor(data)
    n <- nrow(data)

    vars <- c(x, y, z)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z_val <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(z) - 3) * abs(z_val)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_pc <- pc(
    engine = "bnlearn",
    test = my_test_bnlearn,
    alpha = 0.05
  )
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")
})

test_that("custom test works with additional args", {
  my_test <- function(x, y, conditioning_set, suff_stat, args = list()) {
    not_used <- args$not_used
    C <- suff_stat$C
    n <- suff_stat$n

    vars <- c(x, y, conditioning_set)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_suff_stat <- function(data) {
    list(
      C = cor(data),
      n = nrow(data)
    )
  }

  data(tpc_example)

  my_tpc <- tpc(
    engine = "causalDisco",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat,
    args = list(not_used = "This is not used")
  )
  result <- disco(data = tpc_example, method = my_tpc)
  expect_equal(class(result), "Disco")

  my_pc <- pc(
    engine = "pcalg",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat,
    args = list(not_used = "This is not used")
  )
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")

  # bnlearn
  my_test_bnlearn <- function(x, y, z, data, args = list()) {
    not_used <- args$not_used
    C <- cor(data)
    n <- nrow(data)

    vars <- c(x, y, z)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z_val <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(z) - 3) * abs(z_val)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_pc <- pc(
    engine = "bnlearn",
    test = my_test_bnlearn,
    alpha = 0.05,
    args = list(not_used = "This is not used")
  )
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")
})

test_that("custom test respects alpha", {
  my_test <- function(x, y, conditioning_set, suff_stat) {
    C <- suff_stat$C
    n <- suff_stat$n

    vars <- c(x, y, conditioning_set)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_suff_stat <- function(data) {
    list(
      C = cor(data),
      n = nrow(data)
    )
  }

  data(tpc_example)

  my_tpc <- tpc(
    engine = "causalDisco",
    test = my_test,
    alpha = 0.5,
    suff_stat_fun = my_suff_stat
  )
  result <- disco(data = tpc_example, method = my_tpc)
  expect_equal(class(result), "Disco")

  my_pc <- pc(
    engine = "pcalg",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat
  )
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")

  my_test_bnlearn <- function(x, y, conditioning_set, suff_stat) {
    C <- cor(suff_stat)
    n <- nrow(suff_stat)

    vars <- c(x, y, conditioning_set)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }
  my_pc <- pc(
    engine = "bnlearn",
    test = my_test_bnlearn,
    alpha = 0.05
  )
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")

  # Use different naming
  my_test_bnlearn <- function(x, y, z, data) {
    C <- cor(data)
    n <- nrow(data)

    vars <- c(x, y, z)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)
    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
    z_val <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(z) - 3) * abs(z_val)

    pval <- 2 * (1 - pnorm(stat))

    pval
  }

  my_pc <- pc(
    engine = "bnlearn",
    test = my_test_bnlearn,
    alpha = 0.05
  )
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")
})
