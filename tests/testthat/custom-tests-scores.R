test_that("causalDisco tpc and tfci work with custom test", {
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
  result <- disco(data = tpc_example, method = my_tpc)

  expect_equal(class(result), "Disco")

  my_tfci <- tfci(
    engine = "causalDisco",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat
  )
  result <- disco(data = tpc_example, method = my_tfci)

  expect_equal(class(result), "Disco")
})

test_that("pcalg pc and fci work with custom test", {
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

  my_tpc <- pc(
    engine = "pcalg",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat
  )
  result <- disco(data = tpc_example, method = my_tpc)

  expect_equal(class(result), "Disco")

  my_tfci <- fci(
    engine = "pcalg",
    test = my_test,
    alpha = 0.05,
    suff_stat_fun = my_suff_stat
  )
  result <- disco(data = tpc_example, method = my_tfci)

  expect_equal(class(result), "Disco")
})

test_that("bnlearn pc, gs, and iamb family work with custom test", {
  my_bnlearn_test <- function(x, y, z, data, args) {
    suff_stat <- list(C = cor(data), n = nrow(data))

    C <- suff_stat$C
    n <- suff_stat$n

    vars <- c(x, y, z)
    C_sub <- C[vars, vars, drop = FALSE]
    K <- solve(C_sub)

    r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])

    z_stat <- 0.5 * log((1 + r) / (1 - r))

    stat <- sqrt(n - length(z) - 3) * z_stat

    pval <- 2 * (1 - pnorm(abs(stat)))

    c(statistic = stat, p.value = pval)
  }

  data(tpc_example)

  my_pc <- pc(engine = "bnlearn", test = my_bnlearn_test, alpha = 0.05)
  result <- disco(data = tpc_example, method = my_pc)
  expect_equal(class(result), "Disco")

  gs_bnlearn <- gs(
    engine = "bnlearn",
    test = my_bnlearn_test,
    alpha = 0.05
  )
  result <- disco(tpc_example, gs_bnlearn)
  expect_equal(class(result), "Disco")

  iamb_bnlearn <- iamb(engine = "bnlearn", test = my_bnlearn_test, alpha = 0.05)
  result <- disco(data = tpc_example, method = iamb_bnlearn)
  expect_equal(class(result), "Disco")

  iamb_fdr_bnlearn <- iamb_fdr(
    engine = "bnlearn",
    test = my_bnlearn_test,
    alpha = 0.05
  )
  disco(tpc_example, iamb_fdr_bnlearn)
  expect_equal(class(result), "Disco")

  fast_iamb_bnlearn <- fast_iamb(
    engine = "bnlearn",
    test = my_bnlearn_test,
    alpha = 0.05
  )
  result <- disco(data = tpc_example, method = fast_iamb_bnlearn)
  expect_equal(class(result), "Disco")

  inter_iamb_bnlearn <- inter_iamb(
    engine = "bnlearn",
    test = my_bnlearn_test,
    alpha = 0.05
  )
  result <- disco(data = tpc_example, method = inter_iamb_bnlearn)
  expect_equal(class(result), "Disco")
})

