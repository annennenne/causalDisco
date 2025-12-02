test_that(".onLoad works", {
  causalDisco:::.onLoad()
  expect_true(TRUE)
})

test_that("onAttach works", {
  causalDisco:::.onAttach()
  expect_true(TRUE)
})

test_that(".onAttach warns about mismatched heap size", {
  options(java.heap.size = "4g")

  expect_message(
    causalDisco:::.onAttach(),
    regexp = "WARNING: Java heap is 2 GB but you requested 4 GB"
  )
})

test_that(".onAttach does not warn about minimal mismatched heap size", {
  options(java.heap.size = "2050m")

  expect_message(
    causalDisco:::.onAttach(),
    regexp = "Java successfully initialized with 2 GB."
  )
})
