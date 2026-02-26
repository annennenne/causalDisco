test_that("disco warns when using known Tetrad bug about required for fci alg family", {
  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  tetrad_fci <- fci(
    engine = "tetrad",
    test = "conditional_gaussian",
    alpha = 0.05
  )
  expect_warning(
    disco(data = tpc_example, method = tetrad_fci, knowledge = kn),
    "The Tetrad FCI-family algorithms"
  )
})

test_that("disco errors when using known Tetrad bug about required for boss alg family", {
  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  tetrad_fci <- boss(
    engine = "tetrad",
    score = "sem_bic"
  )
  expect_error(
    disco(data = tpc_example, method = tetrad_fci, knowledge = kn),
    "The Tetrad BOSS-family algorithms"
  )
})
