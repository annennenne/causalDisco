test_that("check tetrad install", {
  output <- check_tetrad_install()
  print(output)
  expect_true(TRUE)
})
