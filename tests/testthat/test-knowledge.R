# Knowledge tests

# ──────────────────────────────────────────────────────────────────────────────
# Knowledge generation using mini-dsl
# ──────────────────────────────────────────────────────────────────────────────
testthat::test_that("knowledge object is created correctly using mini-DSL", {
  kn <-
    knowledge(
      tier(
        1 ~ V1 + V2,
        2 ~ V3,
        3 ~ c(V4, V5)
      ),
      forbidden(V1 ~ V3),
      required(V1 ~ V2, V2 ~ V3)
    )
  testthat::expect_equal(kn$vars, tibble(
    var = c("V1", "V2", "V3", "V4", "V5"),
    tier = c(1, 1, 2, 3, 3)
  ))
  testthat::expect_equal(kn$tier_labels, integer(0))
  testthat::expect_equal(kn$frozen, FALSE)
  testthat::expect_equal(kn$edges, tibble(
    status = c("forbidden", "required", "required"),
    from = c("V1", "V1", "V2"),
    to = c("V3", "V2", "V3"),
    tier_from = c(1, 1, 1),
    tier_to = c(2, 1, 2)
  ))
  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c(NA_character_, NA_character_, NA_character_)
  ))
})

# seeding with dataframe
testthat::test_that("seeding knowledge object with a df", {
  df <- data.frame(X1 = 1, X2 = 2, X3 = 3, X4 = 4, check.names = FALSE)
  kn <-
    knowledge(
      df,
      tier(1 ~ X1, 2 ~ X2 + X3),
      required(X1 ~ X2)
    )
  testthat::expect_equal(kn$frozen, TRUE)
  testthat::expect_equal(kn$vars, tibble(
    var = c("X1", "X2", "X3", "X4"),
    tier = c(1, 2, 2, NA)
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Tiers
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier generation with named tiers using character names", {
  kn <- knowledge(
    tier(
      "One" ~ V1 + V2,
      "Two" ~ V3,
      "Three" ~ V4 + V5
    ),
    forbidden(V1 ~ V3),
    required(V1 ~ V2, V2 ~ V3)
  )
  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation with named tiers using symbols/expression", {
  kn <- knowledge(
    tier(
      One ~ V1 + V2,
      Two ~ V3,
      Three ~ V4 + V5
    ),
    forbidden(V1 ~ V3),
    required(V1 ~ V2, V2 ~ V3)
  )
  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation with named tiers using mix of integers, chars, and symbols", {
  kn <- knowledge(
    tier(
      1 ~ V1 + V2,
      Two ~ V3,
      3 ~ V4 + V5,
      "Four" ~ V6,
      Five ~ V7 + V8 + V9
    ),
    forbidden(V1 ~ V3),
    required(V1 ~ V2, V2 ~ V3)
  )
  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3, 4, 5),
    label = c(NA_character_, "Two", NA_character_, "Four", "Five")
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Tiers using verbs only
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(1) |>
    add_tier(2) |>
    add_tier(3) |>
    add_to_tier(2 ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c(NA_character_, NA_character_, NA_character_)
  ))
})

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_tier(Two, after = One) |>
    add_tier(Three, after = Two) |>
    add_to_tier(Two ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_tier(Three, after = One) |>
    add_tier(Two, before = Three) |>
    add_to_tier(Two ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_tier(2) |>
    add_tier(Three, after = 2) |>
    add_to_tier(3 ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", NA_character_, "Three")
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Tiers using verbs and mini-DSL
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier generation with mixing DSL and verbs", {
  kn <- knowledge(
    tier(
      1 ~ V1,
      2 ~ V2
    )
  ) |>
    add_tier(3) |>
    add_to_tier(3 ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c(NA_character_, NA_character_, NA_character_)
  ))
})
testthat::test_that("tier generation with mixing DSL and verbs with symbols", {
  kn <- knowledge(
    tier(
      1 ~ V1,
      2 ~ V2
    )
  ) |>
    add_tier(Three, after = 2) |>
    add_to_tier(Three ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c(NA_character_, NA_character_, "Three")
  ))
})
testthat::test_that("tier generation with mixing DSL and verbs with symbols and chars", {
  kn <- knowledge(
    tier(
      "One" ~ V1,
      Three ~ V2
    )
  ) |>
    add_tier(Two, before = "Three") |>
    add_to_tier("Two" ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation with mixing DSL and verbs with symbols and chars", {
  kn <- knowledge(
    tier(
      "One" ~ V1,
      Three ~ V2
    )
  ) |>
    add_tier(2, after = One) |>
    add_to_tier(2 ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", NA_character_, "Three")
  ))
})

testthat::test_that("tier generation with mixing DSL and verbs with symbols and chars", {
  kn <- knowledge(
    tier(
      "One" ~ V1,
      Three ~ V2
    )
  ) |>
    add_tier(2) |>
    add_to_tier(2 ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", NA_character_, "Three")
  ))
})

testthat::test_that("tier generation with mixing DSL and verbs with symbols and chars", {
  kn <- knowledge(
    tier(
      "One" ~ V1,
      Three ~ V2
    )
  ) |>
    add_tier(Two, after = 1) |>
    add_to_tier(Two ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation with mixing DSL and verbs with symbols and chars", {
  kn <- knowledge(
    tier(
      "One" ~ V1,
      Three ~ V2
    )
  ) |>
    add_tier(Two, before = Three) |>
    add_to_tier(2 ~ V3)

  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3),
    label = c("One", "Two", "Three")
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Tiers using seq_tiers
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier generation using seq_tiers", {
  df <- as.data.frame(
    matrix(runif(10), # 10 random numbers in (0,1)
      nrow = 1,
      ncol = 10,
      byrow = TRUE
    )
  )

  names(df) <- paste0("X_", 1:10) # label the columns X_1 … X_10

  kn <- knowledge(
    df,
    tier(
      seq_tiers(
        1:10,
        ends_with("_{i}")
      )
    ),
    required(X_1 ~ X_2)
  )
  testthat::expect_equal(kn$tiers, tibble(
    idx = 1:10,
    label = rep(NA_character_, 10)
  ))
  testthat::expect_equal(kn$vars, tibble(
    var = paste0("X_", 1:10),
    tier = 1:10
  ))
})

testthat::test_that("tier generation using seq_tiers with labels", {
  df <- data.frame(
    X_1 = 1,
    X_2 = 2,
    tier3_A = 3,
    Y5_ok = 4,
    check.names = FALSE
  )

  kn <- knowledge(
    df,
    tier(
      seq_tiers(1:2, ends_with("_{i}")), # X_1, X_2
      seq_tiers(3, starts_with("tier{i}")), # tier3_…
      seq_tiers(5, matches("Y{i}_")) # exact match
    )
  )
  testthat::expect_equal(kn$tiers, tibble(
    idx = c(1, 2, 3, 5),
    label = c(NA_character_, NA_character_, NA_character_, NA_character_)
  ))
  testthat::expect_equal(kn$vars, tibble(
    var = c("X_1", "X_2", "tier3_A", "Y5_ok"),
    tier = c(1, 2, 3, 5)
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Tiers using 1:n
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier generation using 1:n", {
  df <- as.data.frame(
    matrix(runif(10), # 10 random numbers in (0,1)
      nrow = 1,
      ncol = 10,
      byrow = TRUE
    )
  )

  names(df) <- paste0("X_", 1:10) # label the columns X_1 … X_10

  kn <-
    knowledge(
      df,
      tier(
        1:10
      )
    )
  testthat::expect_equal(kn$tiers, tibble(
    idx = 1:10,
    label = rep(NA_character_, 10)
  ))
  testthat::expect_equal(kn$vars, tibble(
    var = paste0("X_", 1:10),
    tier = 1:10
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Edge errors
# ──────────────────────────────────────────────────────────────────────────────
test_that("forbidden() errors when called without any formulas", {
  expect_error(
    knowledge(
      forbidden()
    ),
    "forbidden() needs at least one two-sided formula.",
    fixed = TRUE
  )
})

test_that("required() errors when called without any formulas", {
  expect_error(
    knowledge(
      required()
    ),
    "required() needs at least one two-sided formula.",
    fixed = TRUE
  )
})
test_that("forbidden() errors when called with a non-formula", {
  expect_error(
    knowledge(
      forbidden(1)
    ),
    "Arguments must be two-sided formulas.",
    fixed = TRUE
  )
})
test_that("required() errors when called with a non-formula", {
  expect_error(
    knowledge(
      required(1)
    ),
    "Arguments must be two-sided formulas.",
    fixed = TRUE
  )
})

test_that("forbidden() and required() errors when no from vars matched", {
  df <- data.frame(V1 = 1, V2 = 2, check.names = FALSE)
  expect_error(
    knowledge(
      df,
      forbidden(1 ~ V1)
    ),
    "Edge selection `1 ~ V1` matched no variables on the left-hand side of the formula.",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      df,
      required(1 ~ V1)
    ),
    "Edge selection `1 ~ V1` matched no variables on the left-hand side of the formula.",
    fixed = TRUE
  )
})

test_that("forbidden() and required() errors when no to vars matched", {
  df <- data.frame(V1 = 1, V2 = 2, check.names = FALSE)
  expect_error(
    knowledge(
      df,
      forbidden(V1 ~ 1)
    ),
    "Edge selection `V1 ~ 1` matched no variables on the right-hand side of the formula.",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      df,
      required(V1 ~ 1)
    ),
    "Edge selection `V1 ~ 1` matched no variables on the right-hand side of the formula.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Tier errors
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier throws error for one variable in two tiers", {
  testthat::expect_error(
    knowledge(
      tier(
        1 ~ V1 + V2,
        Two ~ V1,
      )
    ),
    "Tier specification Two ~ V1 tries to re-assign variable(s) [V1].",
    fixed = TRUE
  )
})

testthat::test_that("tier throws error for using numeric vector without df", {
  testthat::expect_error(
    knowledge(
      tier(
        1:10
      )
    ),
    "Using tier(<numeric vector>) needs the data-frame columns first.",
    fixed = TRUE
  )
})

testthat::test_that("tier() errors when numeric vector length != ncol(df)", {
  df <- data.frame(X1 = 1, X2 = 2, X3 = 3, X4 = 4, check.names = FALSE)
  testthat::expect_error(
    knowledge(
      df,
      tier(
        1:10
      )
    ),
    "Tier vector length (10) must equal number of variables (4).",
    fixed = TRUE
  )
})

test_that("numeric-vector tier() errors on duplicate indices", {
  df <- data.frame(A = 1, B = 2, C = 3, check.names = FALSE)

  expect_error(
    knowledge(
      df,
      tier(1:3), # first time: creates tiers 1,2,3
      tier(1:3) # second time: should detect 1,2,3 already exist
    ),
    "Tier index(es) 1, 2, 3 already exist.",
    fixed = TRUE
  )
})

test_that("tier() throws error when mispecifying tier", {
  df <- data.frame(A = 1, B = 2, C = 3, check.names = FALSE)

  expect_error(
    knowledge(
      df,
      tier(2 ~ 1)
    ),
    "Tier specification 2 ~ 1 matched no variables.",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      df,
      tier(2 ~ X)
    ),
    "Unknown variable(s): X\nThey are not present in the data frame was provided to this knowledge object.",
    fixed = TRUE
  )
  V2 <- 1
  expect_error(
    knowledge(
      tier(
        1 ~ V1,
        V2
      )
    ),
    "Each tier() argument must be a two-sided formula.",
    fixed = TRUE
  )
  # this throws some evaluation error
  expect_error(
    knowledge(
      tier(
        1 ~ V1,
        V4
      )
    )
  )
  df <- data.frame(A = 1, B = 2, check.names = FALSE)

  expect_error(
    knowledge(
      df,
      tier(
        1 ~ starts_with("Z")
      )
    ),
    "Tier specification 1 ~ starts_with(\"Z\") matched no variables.",
    fixed = TRUE
  )
})

test_that("numeric-vector tier() errors on duplicate indices", {
  df <- data.frame(A = 1, B = 2, C = 3, check.names = FALSE)

  expect_error(
    knowledge(
      df,
      tier(1:3), # first time: creates tiers 1,2,3
      tier(1:3) # second time: should detect 1,2,3 already exist
    ),
    "Tier index(es) 1, 2, 3 already exist.",
    fixed = TRUE
  )
})

test_that("seq_tiers() in tier() errors when no variables match the pattern", {
  df <- data.frame(A = 1, B = 2, C = 3, check.names = FALSE)

  expect_error(
    knowledge(
      df,
      tier(
        # build a bundle that will match no columns
        seq_tiers(1, starts_with("zzz"))
      )
    ),
    # literal match of the error message
    "Pattern starts_with(\"zzz\") matched no variables.",
    fixed = TRUE
  )
})

test_that("tier() errors when two seq_tiers patterns overlap", {
  df <- data.frame(
    A = 1,
    B = 2,
    C = 3,
    check.names = FALSE
  )

  expect_error(
    knowledge(
      df,
      tier(
        # seq_tiers(1:2, everything()) produces two formulas
        # 1 ~ everything(), 2 ~ everything()
        # so every column is matched twice → should throw
        seq_tiers(1:2, everything())
      )
    ),
    "Some variables matched by two patterns: A, B, C",
    fixed = TRUE
  )
})

test_that("tier() errors when index is less than 1", {
  expect_error(
    knowledge(
      tier(
        0 ~ V1
      )
    ),
    "Numeric tier must be >= 1.",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      tier(
        -1 ~ V1
      )
    ),
    "Numeric tier must be >= 1.",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      tier(
        -100 ~ V1
      )
    ),
    "Numeric tier must be >= 1.",
    fixed = TRUE
  )
})

test_that("add_tier() errors when index is less than 1", {
  expect_error(
    knowledge() |>
      add_tier(0),
    "Numeric tier must be >= 1.",
    fixed = TRUE
  )
  expect_error(
    knowledge() |>
      add_tier(-1),
    "Numeric tier must be >= 1.",
    fixed = TRUE
  )
  expect_error(
    knowledge() |>
      add_tier(-100),
    "Numeric tier must be >= 1.",
    fixed = TRUE
  )
})
test_that("add_tier() errors when both `before` and `after` are supplied", {
  expect_error(
    knowledge() |>
      add_tier(2, before = 3, after = 1),
    "Cannot supply both `before` and `after`.",
    fixed = TRUE
  )
})

test_that("add_tier() errors when both `before` and `after` are supplied for a labelled tier", {
  expect_error(
    knowledge() |>
      add_tier(1) |>
      add_tier(dumb_tier, before = 1, after = 1),
    "Cannot supply both `before` and `after`.",
    fixed = TRUE
  )
})

test_that("add_tier() errors when either `before` or `after` is less than or larger than the tier, respectively", {
  expect_error(
    knowledge() |>
      add_tier(2) |>
      add_tier(1, after = 2),
    "`after` must be <= `tier`.",
    fixed = TRUE
  )
  expect_error(
    knowledge() |>
      add_tier(1) |>
      add_tier(2, before = 1),
    "`before` must be >= `tier`.",
    fixed = TRUE
  )
})

test_that("add_tier() errors when either `before` or `after` is given but is not in kn$tiers", {
  expect_error(
    knowledge() |>
      add_tier(One, before = Two),
    "`Two` is not a tier label, index, or variable.",
    fixed = TRUE
  )
})

test_that("add_to_tier() errors when tier input is bad", {
  expect_error(
    knowledge() |>
      add_tier(NA),
    "`tier` must be a numeric literal or a non-empty label.",
    fixed = TRUE
  )
  expect_error(
    knowledge() |>
      add_tier(NULL),
    "`tier` must be a numeric literal or a non-empty label.",
    fixed = TRUE
  )
})

test_that("add_tier() errors when no before or after is provided", {
  expect_error(
    knowledge() |>
      add_tier(1) |>
      add_tier(Two),
    "Once the knowledge object already has tiers, supply exactly one of `before` or `after`.",
    fixed = TRUE
  )
  expect_error(
    knowledge() |>
      add_tier(1) |>
      add_tier(Two, after = 1) |>
      add_tier("Three"),
    "Once the knowledge object already has tiers, supply exactly one of `before` or `after`.",
    fixed = TRUE
  )
})

test_that("", {
  # what should happen here?
  kn <- knowledge() |>
    add_tier(1) |>
    add_tier(3) |>
    add_tier(Two, after = 1) |>
    add_tier(Two_and_a_Half, after = Two)
})

# ──────────────────────────────────────────────────────────────────────────────
# Misc errors
# ──────────────────────────────────────────────────────────────────────────────

test_that("knowledge() throws error when using another function than tier(), forbidden(), or required()", {
  df <- data.frame(V1 = 1, V2 = 2, check.names = FALSE)
  expect_error(
    knowledge(
      df,
      musthave(V1 ~ 1)
    ),
    "Only tier(), forbidden(), required() calls are allowed.",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      makingmistakes(V1 ~ 1)
    ),
    "Only tier(), forbidden(), required() calls are allowed.",
    fixed = TRUE
  )
})
