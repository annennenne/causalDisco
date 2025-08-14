test_that("engine guard in check_args_and_distribute_args() rejects unsupported engines", {
  reg <- engine_registry
  expect_error(
    check_args_and_distribute_args(
      search = NULL,
      args   = list(),
      engine = "not-an-engine",
      alg    = "pc"
    ),
    paste0(
      "Engine ", "not-an-engine", " is not supported. Supported engines are: ",
      paste(reg, collapse = ", ")
    ),
    fixed = TRUE
  )
})

test_that("tetrad: requires either test or score, consumes verbose, and covers score-arg branch", {
  skip_if_not_installed("rJava")

  # neither test nor score
  search_t <- TetradSearch$new()
  expect_error(
    check_args_and_distribute_args_tetrad(
      search = search_t,
      args   = list(),
      alg    = "pc",
      test   = NULL,
      score  = NULL
    ),
    "Neither test or score is specified.",
    fixed = TRUE
  )

  # verbose is passed and removed
  search_t <- TetradSearch$new()
  out <- check_args_and_distribute_args_tetrad(
    search = search_t,
    args   = list(verbose = TRUE),
    alg    = "pc",
    test   = "fisher_z",
    score  = NULL
  )
  expect_named(out, c("alg_args", "test_args", "score_args"))
  expect_length(out$alg_args, 0)
  expect_length(out$test_args, 0)
  expect_length(out$score_args, 0)

  # unknown args error
  search_t <- TetradSearch$new()
  expect_error(
    check_args_and_distribute_args_tetrad(
      search = search_t,
      args   = list(.totally_unused = 1),
      alg    = "pc",
      test   = "fisher_z",
      score  = NULL
    ),
    "The following arguments are not used in Tetrad algorithm, test, or score: .totally_unused",
    fixed = TRUE
  )

  # cover the score branch (no need to know actual score args; just exercise the path)
  search_t <- TetradSearch$new()
  out_score <- check_args_and_distribute_args_tetrad(
    search = search_t,
    args   = list(), # empty args is fine; we only need to hit get_parameters_for_function(score = TRUE)
    alg    = "fges",
    test   = NULL,
    score  = "sem_bic"
  )
  expect_named(out_score, c("alg_args", "test_args", "score_args"))
  expect_length(out_score$alg_args, 0)
  expect_length(out_score$test_args, 0)
  expect_true(is.list(out_score$score_args))
})

test_that("pcalg: pc, fci, ges dispatch and unused-arg diagnostics incl. dots warning path", {
  skip_if_not_installed("pcalg")

  # pc branch, known arg passes through
  pc_formals <- names(formals(pcalg::pc))
  known_pc_arg <- if ("m.max" %in% pc_formals) "m.max" else pc_formals[1]
  args_pc <- list()
  args_pc[[known_pc_arg]] <- if (known_pc_arg %in% c("m.max")) 1L else NULL

  out_pc <- check_args_and_distribute_args_pcalg(
    search = NULL,
    args   = args_pc,
    alg    = "pc",
    test   = "fisher_z",
    score  = NULL
  )
  expect_named(out_pc, c("alg_args", "score_args"))
  expect_true(known_pc_arg %in% names(out_pc$alg_args))

  # fci branch covered explicitly
  fci_formals <- names(formals(pcalg::fci))
  known_fci_arg <- if ("na.more" %in% fci_formals) "na.more" else fci_formals[1]
  args_fci <- list()
  args_fci[[known_fci_arg]] <- if (known_fci_arg %in% c("na.more")) TRUE else NULL

  out_fci <- check_args_and_distribute_args_pcalg(
    search = NULL,
    args   = list(),
    alg    = "fci",
    test   = "fisher_z",
    score  = NULL
  )
  expect_named(out_fci, c("alg_args", "score_args"))

  # ges + score path covered; no score args provided, but branch executes
  out_ges <- check_args_and_distribute_args_pcalg(
    search = NULL,
    args   = list(),
    alg    = "ges",
    test   = NULL,
    score  = "sem_bic_int"
  )
  expect_named(out_ges, c("alg_args", "score_args"))

  args_unused <- list(bogus_arg = 123)

  expect_warning(
    check_args_and_distribute_args_pcalg(
      search = NULL,
      args   = args_unused,
      alg    = "ges",
      test   = NULL,
      score  = "sem_bic_int"
    ),
    "The following arguments are not used in pcalg::ges: bogus_arg",
    fixed = TRUE
  )
  expect_error(
    check_args_and_distribute_args_pcalg(
      search = NULL,
      args   = args_unused,
      alg    = "pc",
      test   = "fisher_z",
      score  = NULL
    ),
    "The following arguments are not used in pcalg::pc: bogus_arg",
    fixed = TRUE
  )

  # unsupported alg exact message
  expect_error(
    check_args_and_distribute_args_pcalg(
      search = NULL,
      args   = list(),
      alg    = "not-an-alg",
      test   = NULL,
      score  = NULL
    ),
    "Unsupported algorithm:not-an-alg",
    fixed = TRUE
  )
})

test_that("bnlearn: algorithm existence, dots handling, and passthrough", {
  skip_if_not_installed("bnlearn")

  expect_error(
    check_args_and_distribute_args_bnlearn(
      search = NULL,
      args = list(),
      alg = "not-a-bnlearn-alg",
      allow_dots = FALSE
    ),
    "Unsupported algorithm: not-a-bnlearn-alg",
    fixed = TRUE
  )

  candidates <- c(
    "pc.stable", "gs", "iamb", "fast.iamb", "inter.iamb", "iamb.fdr",
    "mmpc", "si.hiton.pc", "hpc", "hc", "tabu", "mmhc", "rsmax2", "h2pc",
    "chow.liu", "aracne"
  )
  has_fun <- vapply(
    candidates,
    function(a) exists(a, envir = asNamespace("bnlearn")),
    logical(1)
  )
  candidates <- candidates[has_fun]
  formals_list <- lapply(candidates, function(a) names(formals(get(a, envir = asNamespace("bnlearn")))))
  names(formals_list) <- candidates

  no_dots <- names(Filter(function(x) !("..." %in% x), formals_list))
  with_dots <- names(Filter(function(x) "..." %in% x, formals_list))

  if (length(no_dots) > 0) {
    nd <- no_dots[[1]]
    expect_error(
      check_args_and_distribute_args_bnlearn(
        search = NULL,
        args = list(unused_extra = 1),
        alg = nd,
        allow_dots = FALSE
      ),
      paste0("The following arguments are not valid for bnlearn::", nd, ": unused_extra"),
      fixed = TRUE
    )
  }

  if (length(with_dots) > 0) {
    wd <- with_dots[[1]]
    expect_error(
      check_args_and_distribute_args_bnlearn(
        search = NULL,
        args = list(unclaimed = 42),
        alg = wd,
        allow_dots = FALSE
      ),
      paste0(
        "bnlearn::", wd, " has a '...' formal, but these arguments are not ",
        "recognised: unclaimed",
        ".  Set allow_dots = TRUE if you really want to forward them."
      ),
      fixed = TRUE
    )
    res <- check_args_and_distribute_args_bnlearn(
      search = NULL,
      args = list(unclaimed = 42),
      alg = wd,
      allow_dots = TRUE
    )
    expect_equal(res$unclaimed, 42)
  }

  any_alg <- candidates[[1]]
  knowns <- formals_list[[any_alg]]
  safe_args <- list()
  if (length(knowns) > 0 && knowns[[1]] != "...") {
    safe_args[[knowns[[1]]]] <- NULL
  }
  res2 <- check_args_and_distribute_args_bnlearn(
    search = NULL,
    args = safe_args,
    alg = any_alg,
    allow_dots = FALSE
  )
  expect_identical(res2, safe_args)
})

test_that("top-level dispatcher routes to each engine helper", {
  skip_if_not_installed("pcalg")
  skip_if_not_installed("bnlearn")
  skip_if_not_installed("rJava")

  out_t <- check_args_and_distribute_args(
    search = TetradSearch$new(),
    args   = list(verbose = TRUE),
    engine = "tetrad",
    alg    = "pc",
    test   = "fisher_z",
    score  = NULL
  )
  expect_named(out_t, c("alg_args", "test_args", "score_args"))

  out_p <- check_args_and_distribute_args(
    search = NULL,
    args   = list(m.max = 1L),
    engine = "pcalg",
    alg    = "pc",
    test   = "fisher_z",
    score  = NULL
  )
  expect_named(out_p, c("alg_args", "score_args"))

  out_b <- check_args_and_distribute_args(
    search = NULL,
    args   = list(),
    engine = "bnlearn",
    alg    = "pc"
  )
  expect_identical(out_b, list())
})
