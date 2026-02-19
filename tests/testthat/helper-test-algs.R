# Wrapper to create the algorithm object with optional extra args
make_alg <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  main_args <- list(engine = engine)
  if (!is.null(test)) {
    main_args$test <- test
  }
  if (!is.null(score)) {
    main_args$score <- score
  }

  all_args <- c(main_args, alg_args, test_args)
  do.call(alg_fun, all_args)
}

# Generalized wrapper to create the algorithm object
make_alg <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  # Build the main argument list
  main_args <- list(engine = engine)
  if (!is.null(test)) {
    main_args$test <- test
  }
  if (!is.null(score)) {
    main_args$score <- score
  }

  # Combine with additional arguments
  all_args <- c(main_args, alg_args, test_args)
  do.call(alg_fun, all_args)
}

# Generalized test for tier knowledge
test_tier_knowledge <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  test_that(
    paste0(
      deparse(substitute(alg_fun)),
      " (",
      engine,
      ", test=",
      test,
      ", score=",
      score,
      ") respects tier knowledge"
    ),
    {
      data(tpc_example)

      # Using names as tiers
      kn <- knowledge(
        tpc_example,
        tier(
          child ~ starts_with("child"),
          youth ~ starts_with("youth"),
          old ~ starts_with("old")
        )
      )
      output <- disco(
        data = tpc_example,
        method = make_alg(alg_fun, engine, test, score, alg_args, test_args),
        knowledge = kn
      )
      edges <- output$caugi@edges
      violations <- causalDisco:::check_tier_violations(edges, kn)
      expect_true(
        nrow(violations) == 0,
        info = "Tier violations were found in the output graph."
      )

      # Using numeric tiers
      kn_numeric <- knowledge(
        tpc_example,
        tier(
          1 ~ starts_with("old"),
          2 ~ starts_with("youth"),
          3 ~ starts_with("child")
        )
      )
      output <- disco(
        data = tpc_example,
        method = make_alg(alg_fun, engine, test, score, alg_args, test_args),
        knowledge = kn_numeric
      )
      edges <- output$caugi@edges
      violations <- causalDisco:::check_tier_violations(edges, kn_numeric)
      expect_true(
        nrow(violations) == 0,
        info = "Tier violations were found in the output graph."
      )
    }
  )
}

# Generalized test for required knowledge
test_required_knowledge <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  test_that(
    paste0(
      deparse(substitute(alg_fun)),
      " (",
      engine,
      ", test=",
      test,
      ", score=",
      score,
      ") respects required background knowledge"
    ),
    {
      data(tpc_example)

      # Required edge only
      kn <- knowledge(tpc_example, child_x1 %-->% youth_x3)
      output <- disco(
        tpc_example,
        make_alg(alg_fun, engine, test, score, alg_args, test_args),
        knowledge = kn
      )
      edges <- output$caugi@edges
      violations <- causalDisco:::check_edge_constraints(edges, kn)
      expect_true(
        nrow(violations) == 0,
        info = "Required edge not found in the output graph."
      )

      # Required + tier knowledge
      kn <- knowledge(
        tpc_example,
        tier(
          child ~ starts_with("child"),
          youth ~ starts_with("youth"),
          old ~ starts_with("old")
        ),
        youth_x3 %-->% oldage_x5
      )
      output <- disco(
        tpc_example,
        make_alg(alg_fun, engine, test, score, alg_args, test_args),
        knowledge = kn
      )
      edges <- output$caugi@edges
      expect_true(
        nrow(causalDisco:::check_tier_violations(edges, kn)) == 0,
        info = "Tier violations found."
      )
      expect_true(
        nrow(causalDisco:::check_edge_constraints(edges, kn)) == 0,
        info = "Required edge not found."
      )
    }
  )
}

# Generalized test for forbidden knowledge
test_forbidden_knowledge <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  test_that(
    paste0(
      deparse(substitute(alg_fun)),
      " (",
      engine,
      ", test=",
      test,
      ", score=",
      score,
      ") respects forbidden background knowledge"
    ),
    {
      data(tpc_example)

      kn <- knowledge(
        tpc_example,
        child_x1 %!-->% youth_x3,
        child_x2 %!-->% child_x1
      )
      output <- disco(
        tpc_example,
        make_alg(alg_fun, engine, test, score, alg_args, test_args),
        knowledge = kn
      )
      edges <- output$caugi@edges
      violations <- causalDisco:::check_edge_constraints(edges, kn)
      expect_true(
        nrow(violations) == 0,
        info = "Forbidden edge found in the output graph."
      )
    }
  )
}

# Test that additional algorithm arguments work
test_additional_alg_args <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  test_that(
    paste0(
      deparse(substitute(alg_fun)),
      " (",
      engine,
      ", test=",
      test,
      ", score=",
      score,
      ") works with additional algorithm args"
    ),
    {
      data(num_data)
      out <- disco(
        num_data,
        make_alg(
          alg_fun,
          engine,
          test = test,
          score = score,
          alg_args = alg_args,
          test_args = test_args
        )
      )
      expect_equal(class(out), "Disco")
    }
  )
}

# Test that additional test/score arguments work
test_additional_test_or_score_args <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  test_that(
    paste0(
      deparse(substitute(alg_fun)),
      " (",
      engine,
      ", test=",
      test,
      ", score=",
      score,
      ") works with additional test/score args"
    ),
    {
      data(num_data)
      out <- disco(
        num_data,
        make_alg(
          alg_fun,
          engine,
          test = test,
          score = score,
          alg_args = alg_args,
          test_args = test_args
        )
      )
      expect_equal(class(out), "Disco")
    }
  )
}


# Master helper to run all standard tests for an algorithm
run_all_tests <- function(
  alg_fun,
  engine,
  test = NULL,
  score = NULL,
  alg_args = list(),
  test_args = list()
) {
  test_tier_knowledge(alg_fun, engine, test, score, alg_args, test_args)
  test_required_knowledge(alg_fun, engine, test, score, alg_args, test_args)
  test_forbidden_knowledge(alg_fun, engine, test, score, alg_args, test_args)
  if (length(alg_args) > 0) {
    test_additional_alg_args(alg_fun, engine, test, score, alg_args)
  }
  if (length(test_args) > 0) {
    test_additional_test_or_score_args(alg_fun, engine, test, score, test_args)
  }
}
