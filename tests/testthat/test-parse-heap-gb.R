# ──────────────────────────────────────────────────────────────────────────────
# parse_heap_gb.R
# ──────────────────────────────────────────────────────────────────────────────

test_that("parse_heap_gb converts valid strings", {
  expect_equal(parse_heap_gb("2g"), 2)
  expect_equal(parse_heap_gb("2gb"), 2)
  expect_equal(parse_heap_gb("2048m"), 2)
  expect_equal(parse_heap_gb("2048mb"), 2)
  # Test with uppercase letters
  expect_equal(parse_heap_gb("2G"), 2)
  expect_equal(parse_heap_gb("2GB"), 2)
  expect_equal(parse_heap_gb("2048M"), 2)
  expect_equal(parse_heap_gb("2048MB"), 2)
})

test_that("parse_heap_gb rejects invalid strings", {
  expect_error(parse_heap_gb("2"), "not recognised")
  expect_error(parse_heap_gb("two"), "not recognised")
  expect_error(parse_heap_gb("2gbb"), "not recognised")
})
