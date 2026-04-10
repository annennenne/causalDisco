test_that("Various errors", {
  data(num_data)

  my_tpc <- tpc(test = "fisher_z_mi")
  expect_error(
    disco(num_data, my_tpc),
    "fisher_z_mi requires a mids object.",
    fixed = TRUE
  )

  expect_error(
    constraint_based_prepare_inputs(
      data = num_data,
      knowledge = NULL,
      varnames = NULL,
      na_method = "none",
      test = structure(list(), missing_mode = "mi"),
      suff_stat = NULL,
      directed_as_undirected = FALSE,
      function_name = "test_function"
    ),
    "Selected CI test requires a 'mids' object (multiple imputation data).",
    fixed = TRUE
  )

  expect_error(
    constraint_based_prepare_inputs(
      data = NULL,
      knowledge = NULL,
      varnames = NULL,
      na_method = "none",
      test = structure(list(), missing_mode = "none"),
      suff_stat = NULL,
      directed_as_undirected = FALSE,
      function_name = "test_function"
    ),
    "Either data or sufficient statistic must be supplied.",
    fixed = TRUE
  )

  expect_error(
    constraint_based_prepare_inputs(
      data = NULL,
      knowledge = NULL,
      varnames = NULL,
      na_method = "none",
      test = structure(list(), missing_mode = "none"),
      suff_stat = 1,
      directed_as_undirected = FALSE,
      function_name = "test_function"
    ),
    "Unknown suff_stat format; cannot infer variable names.",
    fixed = TRUE
  )
})
