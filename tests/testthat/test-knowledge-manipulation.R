# ──────────────────────────────────────────────────────────────────────────────
# reorder_tiers()
# ──────────────────────────────────────────────────────────────────────────────

# helper to avoid repetition
tiers_tbl <- function(...) tibble::tibble(label = c(...))

test_that("reorder_tiers() works with complete permutations", {
  kn <- knowledge(tier(One ~ V1, Two ~ V2, Three ~ V3))

  # by label, character
  expect_equal(
    reorder_tiers(kn, c("One", "Three", "Two"))$tiers,
    tiers_tbl("One", "Three", "Two")
  )

  # by label, bare symbols
  expect_equal(
    reorder_tiers(kn, c(One, Three, Two))$tiers,
    tiers_tbl("One", "Three", "Two")
  )

  # by index
  expect_equal(
    reorder_tiers(kn, c(1, 3, 2), by_index = TRUE)$tiers,
    tiers_tbl("One", "Three", "Two")
  )
})

test_that("reorder_tiers() errors on incomplete or duplicated permutations", {
  kn <- knowledge(tier(One ~ V1, Two ~ V2, Three ~ V3))

  expect_error(
    reorder_tiers(kn, c("One", "Two")),
    "`order` must list every existing tier exactly once",
    fixed = TRUE
  )
  expect_error(
    reorder_tiers(kn, c("One", "One", "Two")),
    "`order` must list every existing tier exactly once",
    fixed = TRUE
  )
  expect_error(
    reorder_tiers(kn, c(1, 1, 2), by_index = TRUE),
    "`order` must be a permutation of 1:3 when `by_index = TRUE`.",
    fixed = TRUE
  )
})

test_that("reorder_tiers() handles numeric, character and bad elements", {
  kn_num <- knowledge(
    tibble::tibble(A = 1, B = 2),
    tier(1 ~ A),
    tier(2 ~ B)
  )

  kn_chr <- knowledge(
    tibble::tibble(X = 1, Y = 2),
    tier("a" ~ X),
    tier("b" ~ Y)
  )
  kn_num2 <- reorder_tiers(kn_num, c(2, 1)) # numeric literals

  expect_equal(kn_num2$tiers$label, c("2", "1"))

  kn_chr2 <- reorder_tiers(kn_chr, c("b", "a")) # quoted strings

  expect_equal(kn_chr2$tiers$label, c("b", "a"))

  # 1:2 is a language call, so as_label1() must raise the custom error
  expect_error(
    reorder_tiers(kn_chr, c(a, 1:2)), # 'a' is fine, 1:2 is not
    "`order` contains an unsupported element",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# reposition_tier()
# ──────────────────────────────────────────────────────────────────────────────

test_that("reposition_tier() moves a tier before/after another", {
  kn <- knowledge(tier(One ~ V1, Two ~ V2, Three ~ V3))

  expect_equal(
    reposition_tier(kn, Three, before = Two)$tiers,
    tiers_tbl("One", "Three", "Two")
  )
  expect_equal(
    reposition_tier(kn, "Three", before = "Two")$tiers,
    tiers_tbl("One", "Three", "Two")
  )
  expect_equal(
    reposition_tier(kn, Three, after = One)$tiers,
    tiers_tbl("One", "Three", "Two")
  )
  expect_equal(
    reposition_tier(kn, "Three", after = "One")$tiers,
    tiers_tbl("One", "Three", "Two")
  )

  # by index
  expect_equal(
    reposition_tier(kn, 3, before = 2, by_index = TRUE)$tiers,
    tiers_tbl("One", "Three", "Two")
  )
  expect_equal(
    reposition_tier(kn, 3, after = 1, by_index = TRUE)$tiers,
    tiers_tbl("One", "Three", "Two")
  )
})

test_that("reposition_tier() validates inputs", {
  kn <- knowledge(tier(One ~ V1, Two ~ V2, Three ~ V3))

  # both before and after supplied
  expect_error(
    reposition_tier(kn, Three, before = One, after = Two),
    "exactly one of",
    fixed = TRUE
  )

  # unknown tier
  expect_error(
    reposition_tier(kn, Four, before = One),
    "does not exist",
    fixed = TRUE
  )

  # unknown anchor
  expect_error(
    reposition_tier(kn, Three, before = Four),
    "does not exist",
    fixed = TRUE
  )
})

test_that("reposition_tier() errors when no before or after is provided", {
  kn <- knowledge(tier(One ~ V1, Two ~ V2, Three ~ V3))
  expect_error(
    reposition_tier(kn, One),
    "Supply exactly one of `before` or `after`.",
    fixed = TRUE
  )
})

test_that("reposition_tier() edge cases", {
  kn <- knowledge(
    tibble::tibble(V1 = 1, V2 = 2, V3 = 3),
    tier(1 ~ V1),
    tier(2 ~ V2),
    tier(3 ~ V3)
  )

  # by_index = TRUE but anchor has length > 1
  expect_error(
    reposition_tier(kn, tier = 1, after = c(2, 3), by_index = TRUE),
    "length-1 numeric",
    fixed = TRUE
  )

  # numeric literal resolves to a character label and reorder succeeds
  kn2 <- reposition_tier(kn, tier = 2, before = 1)
  expect_equal(kn2$tiers$label, c("2", "1", "3"))

  # invalid tier reference triggers custom error
  expect_error(
    reposition_tier(kn, tier = 1:2, after = 1),
    "Tier reference .* is invalid",
    perl = TRUE
  )

  # tier identical to anchor returns object unchanged
  expect_identical(
    reposition_tier(kn, tier = 1, before = 1),
    kn
  )
})
