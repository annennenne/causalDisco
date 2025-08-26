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
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = c("V1", "V2", "V3", "V4", "V5"),
    tier = c("1", "1", "2", "3", "3")
  ))
  testthat::expect_equal(kn$frozen, FALSE)
  testthat::expect_equal(kn$edges, tibble::tibble(
    status = c("forbidden", "required", "required"),
    from = c("V1", "V1", "V2"),
    to = c("V3", "V2", "V3"),
    tier_from = c("1", "1", "1"),
    tier_to = c("2", "1", "2")
  ))
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "2", "3")
  ))
})

# seeding with dataframe
testthat::test_that("seeding knowledge object with a df, matrix, or tibble works", {
  df <- data.frame(X1 = 1, X2 = 2, X3 = 3, X4 = 4, check.names = FALSE)
  tbl <- tibble::as_tibble(df)
  mat <- as.matrix(df)
  kn <-
    knowledge(
      df,
      tier(1 ~ X1, 2 ~ X2 + X3),
      required(X1 ~ X2)
    )
  kn_tbl <- knowledge(
    tbl,
    tier(1 ~ X1, 2 ~ X2 + X3),
    required(X1 ~ X2)
  )
  kn_mat <- knowledge(
    mat,
    tier(1 ~ X1, 2 ~ X2 + X3),
    required(X1 ~ X2)
  )
  testthat::expect_equal(kn, kn_tbl)
  testthat::expect_equal(kn, kn_mat)
  testthat::expect_equal(kn$frozen, TRUE)
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = c("X1", "X2", "X3", "X4"),
    tier = c("1", "2", "2", NA_character_)
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
  testthat::expect_equal(kn$tiers, tibble::tibble(
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
  testthat::expect_equal(kn$tiers, tibble::tibble(
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
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "Two", "3", "Four", "Five")
  ))
})

testthat::test_that("tier generation with negative numeric tiers errors", {
  testthat::expect_error(
    knowledge(
      tier(
        -1 ~ V1 + V2,
      )
    ),
    "`tier` must be a single non-empty label or a non-negative numeric literal.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Tiers using verbs only
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(1) |>
    add_tier(2, after = 1) |>
    add_tier(3, after = 2) |>
    add_to_tier(2 ~ V3)

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "2", "3")
  ))
})

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_tier(Two, after = One) |>
    add_tier(Three, after = Two) |>
    add_to_tier(Two ~ V3)

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_tier(Three, after = One) |>
    add_tier(Two, before = Three) |>
    add_to_tier(Two ~ V3)

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One", "Two", "Three")
  ))
})

testthat::test_that("tier generation using verbs only", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_tier(2, after = One) |>
    add_tier(Three, after = 2) |>
    add_to_tier(Three ~ V3)

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One", "2", "Three")
  ))
})

test_that("tier generation with verbs works", {
  kn <- knowledge() |>
    add_tier(1) |>
    add_tier(3, after = 1) |>
    add_tier(Two, before = 3) |>
    add_tier(Two_and_a_Half, after = Two) |>
    add_tier(2.75, before = 3)
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "Two", "Two_and_a_Half", "2.75", "3")
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
    add_tier(3, after = 2) |>
    add_to_tier(3 ~ V3)

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "2", "3")
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

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "2", "Three")
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

  testthat::expect_equal(kn$tiers, tibble::tibble(
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

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One", "2", "Three")
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

  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One", "2", "Three")
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
    add_to_tier(Two ~ V3)

  testthat::expect_equal(kn$tiers, tibble::tibble(
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
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = 1:10 |> as.character()
  ))
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = paste0("X_", 1:10),
    tier = 1:10 |> as.character()
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
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("1", "2", "3", "5")
  ))
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = c("X_1", "X_2", "tier3_A", "Y5_ok"),
    tier = c(1, 2, 3, 5) |> as.character()
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
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = 1:10 |> as.character()
  ))
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = paste0("X_", 1:10),
    tier = 1:10 |> as.character()
  ))
})


# ──────────────────────────────────────────────────────────────────────────────
# Add to tier verb
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("add_to_tier() works as expected", {
  kn <- knowledge() |>
    add_tier(One) |>
    add_to_tier(One ~ V1 + V2)
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One")
  ))
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = c("V1", "V2"),
    tier = c("One", "One")
  ))
})

testthat::test_that("add_to_tier() works as expected with mini-DSL", {
  kn <- knowledge(
    tier(
      One ~ V1 + V2,
      2 ~ V3 + V4,
      "Three" ~ V5
    )
  ) |>
    add_to_tier(One ~ V6)
  testthat::expect_equal(kn$tiers, tibble::tibble(
    label = c("One", "2", "Three")
  ))
  testthat::expect_equal(kn$vars, tibble::tibble(
    var = c("V1", "V2", "V6", "V3", "V4", "V5"),
    tier = c("One", "One", "One", "2", "2", "Three")
  ))
})

# ──────────────────────────────────────────────────────────────────────────────
# Edge errors
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("forbidden() errors when called without any formulas", {
  expect_error(
    knowledge(
      forbidden()
    ),
    "forbidden() needs at least one two-sided formula.",
    fixed = TRUE
  )
})

testthat::test_that("required() errors when called without any formulas", {
  expect_error(
    knowledge(
      required()
    ),
    "required() needs at least one two-sided formula.",
    fixed = TRUE
  )
})
testthat::test_that("forbidden() errors when called with a non-formula", {
  expect_error(
    knowledge(
      forbidden(1)
    ),
    "Arguments must be two-sided formulas.",
    fixed = TRUE
  )
})
testthat::test_that("required() errors when called with a non-formula", {
  expect_error(
    knowledge(
      required(1)
    ),
    "Arguments must be two-sided formulas.",
    fixed = TRUE
  )
})

testthat::test_that("forbidden() and required() errors when no from vars matched", {
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

testthat::test_that("forbidden() and required() errors when no to vars matched", {
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
testthat::test_that("tier() errors if no formulas are supplied", {
  df <- tibble::tibble(V1 = 1, V2 = 2)

  expect_error(
    knowledge(df, tier()),
    "tier() needs at least one two-sided formula.",
    fixed = TRUE
  )
})

test_that("add_tier() errors for an empty tier label", {
  kn <- knowledge()

  expect_error(
    add_tier(kn, ""), # invalid label
    "`tier` must be a non-empty label.",
    fixed = TRUE
  )
})

test_that("add_tier() errors when the label already exists", {
  kn <- knowledge() |> add_tier(1) # creates tier "1"

  expect_error(
    add_tier(kn, 1), # duplicate
    "Tier label `1` already exists.",
    fixed = TRUE
  )
})

test_that("add_tier() errors if the anchor tier is missing", {
  kn <- knowledge() |> add_tier("first") # only one tier so far

  expect_error(
    add_tier(kn, "second", after = "ghost"), # bad anchor
    "`ghost` does not refer to an existing tier.",
    fixed = TRUE
  )
})

testthat::test_that("add_to_tier() errors when adding existing variable to another tier", {
  testthat::expect_error(
    knowledge(
      tier(
        One ~ V1 + V2,
        2 ~ V3 + V4,
        "Three" ~ V5
      )
    ) |>
      add_to_tier(One ~ V3 + V4),
    "Cannot reassign variable(s) [V3, V4] to tier `One` using add_to_tier().",
    fixed = TRUE
  )
  testthat::expect_error(
    knowledge(
      tier(
        One ~ V1 + V2,
        2 ~ V3 + V4,
        "Three" ~ V5
      )
    ) |>
      add_to_tier(2 ~ V3 + V1),
    "Cannot reassign variable(s) [V1] to tier `2` using add_to_tier().",
    fixed = TRUE
  )
})

test_that("add_to_tier() errors if no formulas are supplied", {
  kn <- knowledge()

  expect_error(
    add_to_tier(kn), # no ...
    "add_to_tier() needs at least one two-sided formula.",
    fixed = TRUE
  )
})

test_that("add_to_tier() errors when a non-formula argument is given", {
  kn <- knowledge()

  expect_error(
    add_to_tier(kn, "oops"), # not a formula
    "Each argument must be a two-sided formula.",
    fixed = TRUE
  )
})

test_that("add_to_tier() errors when the target tier does not exist", {
  kn <- knowledge() # no tiers yet

  expect_error(
    add_to_tier(kn, ghost ~ V1), # lhs tier unknown
    "Tier `ghost` does not exist. Create it first with add_tier().",
    fixed = TRUE
  )
})

test_that("add_to_tier() errors when RHS matches no variables", {
  kn <- knowledge() |> add_tier("T1") # create tier

  # tidyselect call that matches nothing because kn$vars is still empty
  expect_error(
    add_to_tier(kn, T1 ~ starts_with("foo")),
    "matched no variables",
    fixed = FALSE
  )
})

testthat::test_that("tier throws error for one variable in two tiers", {
  testthat::expect_error(
    knowledge(
      tier(
        1 ~ V1 + V2,
        Two ~ V1,
      )
    ),
    "Tier specification Two ~ V1 tries to re-assign variable(s) [V1] to a new tier.",
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

testthat::test_that("numeric-vector tier() errors on duplicate indices", {
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

testthat::test_that("tier() throws error when mispecifying tier", {
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
    "Unknown variable(s): [X]
They are not present in the data frame provided to this knowledge object.",
    fixed = TRUE
  )

  expect_error(
    knowledge(
      data.frame(V1 = 1, V2 = 2, check.names = FALSE),
      tier(
        1 ~ V1,
        "V2"
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

testthat::test_that("numeric-vector tier() errors on duplicate indices", {
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

testthat::test_that("seq_tiers() in tier() errors when no variables match the pattern", {
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

testthat::test_that("tier() errors when two seq_tiers patterns overlap", {
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

testthat::test_that("seq_tiers() placeholder validation and numeric default branch", {
  expect_error(
    seq_tiers(1:2, foo), # no placeholder
    "`vars` must contain the placeholder `i`",
    fixed = TRUE
  )

  fml <- seq_tiers(1:2, c(i, 42))

  expect_length(fml, 2)
  rhs_txt <- rlang::expr_text(rlang::f_rhs(fml[[1]]))

  expect_true(grepl("\\b1\\b", rhs_txt)) # i → "1"
  expect_true(grepl("\\b42\\b", rhs_txt)) # 42 unchanged
})


testthat::test_that("add_tier() errors when both `before` and `after` are supplied", {
  expect_error(
    knowledge() |>
      add_tier(2, before = 3, after = 1),
    "Cannot supply both `before` and `after`.",
    fixed = TRUE
  )
})

testthat::test_that("add_tier() errors when both `before` and `after` are supplied for a labelled tier", {
  expect_error(
    knowledge() |>
      add_tier(1) |>
      add_tier(dumb_tier, before = 1, after = 1),
    "Cannot supply both `before` and `after`.",
    fixed = TRUE
  )
})

testthat::test_that("add_tier() errors when either `before` or `after` is given but is not in kn$tiers", {
  expect_error(
    knowledge() |>
      add_tier(One, before = Two),
    "`before`/`after` cannot be used when there are no existing tiers.",
    fixed = TRUE
  )
})

testthat::test_that("add_to_tier() errors when tier input is bad", {
  expect_error(
    knowledge() |>
      add_tier(NA),
    "`tier` must be a single non-empty label or a non-negative numeric literal.",
    fixed = TRUE
  )
  expect_error(
    knowledge() |>
      add_tier(NULL),
    "`tier` must be a single non-empty label or a non-negative numeric literal.",
    fixed = TRUE
  )
})

testthat::test_that("add_tier() errors when no before or after is provided", {
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

test_that("tier() attaches to an existing tier label", {
  df <- tibble::tibble(V1 = 1, V2 = 2)

  kn <- knowledge(
    df,
    tier(1 ~ V1), # creates tier "1" and assigns V1
    tier(1 ~ V2) # same label, should hit the 'add_to_tier' path
  )

  # only one tier row exists and it is labelled "1"
  expect_equal(kn$tiers$label, "1")

  # both variables now belong to that tier
  expect_setequal(
    kn$vars |> dplyr::filter(tier == "1") |> dplyr::pull(var),
    c("V1", "V2")
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# reorder_tiers()
# ──────────────────────────────────────────────────────────────────────────────

# helper to avoid repetition
tiers_tbl <- function(...) tibble::tibble(label = c(...))

testthat::test_that("reorder_tiers() works with complete permutations", {
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

testthat::test_that("reorder_tiers() errors on incomplete or duplicated permutations", {
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

testthat::test_that("reposition_tier() moves a tier before/after another", {
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

testthat::test_that("reposition_tier() validates inputs", {
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

testthat::test_that("reposition_tier() errors when no before or after is provided", {
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


# ──────────────────────────────────────────────────────────────────────────────
# Tier violations
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("reordering respects tier-violation rules", {
  # only forbidden edge → any reorder is fine
  kn <- knowledge(
    tier(One ~ V1, Two ~ V2, Three ~ V3),
    forbidden(V2 ~ V3)
  )
  expect_silent(reorder_tiers(kn, c("Three", "One", "Two")))
  expect_silent(reposition_tier(kn, Three, before = One))

  # required edge → illegal uphill move must error
  kn2 <- knowledge(
    tier(One ~ V1, Two ~ V2, Three ~ V3),
    required(V2 ~ V3) # V2 must stay *before* V1
  )

  expect_error(reposition_tier(kn2, Three, after = One),
    "Edge(s) violate tier ordering: V2 --> V3",
    fixed = TRUE
  )

  expect_error(
    reorder_tiers(kn2, c("One", "Three", "Two")),
    "Edge(s) violate tier ordering: V2 --> V3",
    fixed = TRUE
  )
})

testthat::test_that("adding tier after required edge is provided will trigger tier violation error", {
  expect_error(
    knowledge(
      required(V2 ~ V1)
    ) |>
      add_tier(1) |>
      add_tier(2, after = 1) |>
      add_to_tier(2 ~ V2) |>
      add_to_tier(1 ~ V1),
    "Edge(s) violate tier ordering: V2 --> V1",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Misc errors
# ──────────────────────────────────────────────────────────────────────────────
test_that("unfreeze() clears the frozen flag", {
  df <- tibble::tibble(A = 1, B = 2)

  kn_frozen <- knowledge(df) # passing a data frame sets frozen = TRUE
  expect_true(kn_frozen$frozen)

  kn_unfrozen <- unfreeze(kn_frozen)
  expect_false(kn_unfrozen$frozen)
})

test_that("unfreeze() allows adding new variables", {
  df <- tibble::tibble(A = 1, B = 2)
  kn <- knowledge(df) |> unfreeze() # thaw the object

  # add a new variable that wasn't in the original data frame
  kn2 <- add_vars(kn, "C")

  expect_setequal(kn2$vars$var, c("A", "B", "C"))
  expect_false(kn2$frozen) # flag stays FALSE
})
testthat::test_that("knowledge() throws error when using another function than tier(), forbidden(), or required()", {
  df <- data.frame(V1 = 1, V2 = 2, check.names = FALSE)
  expect_error(
    knowledge(
      df,
      musthave(V1 ~ 1)
    ),
    "Only tier(), forbidden(), required(), and exogenous()",
    fixed = TRUE
  )
  expect_error(
    knowledge(
      makingmistakes(V1 ~ 1)
    ),
    "Only tier(), forbidden(), required(), and exogenous()",
    fixed = TRUE
  )
})

test_that("print.knowledge() snapshot", {
  local_edition(3) # enable v3 snapshotting

  withr::with_options(list(
    crayon.enabled = FALSE, # strip colour codes
    cli.num_colors = 1
  ), {
    kn <- knowledge(
      tibble::tibble(V1 = 1, V2 = 2),
      tier(1 ~ V1),
      tier(2 ~ V2),
      forbidden(V1 ~ V2)
    )

    expect_snapshot_output(print(kn), cran = FALSE)
  })
})

test_that(".edge_verb() validates formula structure and matches", {
  kn <- knowledge() # empty, so no vars are known yet

  # not a two-sided formula
  expect_error(
    .edge_verb(kn, "forbidden", rlang::quo(V1)),
    "Edge specification must be a two-sided formula",
    fixed = TRUE
  )

  # both sides match zero vars: specific error branch
  expect_error(
    .edge_verb(
      kn, "forbidden", rlang::quo(starts_with("Z") ~ starts_with("W"))
    ),
    "Formula `starts_with(\"Z\") ~ starts_with(\"W\")` matched no variables.",
    fixed = TRUE
  )
})


testthat::test_that(".vars_from_spec() handles c(...) and symbol fallback paths", {
  kn <- knowledge(tibble::tibble(V1 = 1, V2 = 2))

  # unsupported argument inside c()
  expect_error(
    .vars_from_spec(kn, quote(c(V1, 42))),
    "Unsupported argument in c\\(\\):",
    perl = TRUE
  )

  # symbol resolves to a user-supplied character vector
  local_vec <- c("V1", "V2")
  expect_equal(
    .vars_from_spec(kn, quote(local_vec)),
    local_vec
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# + operator
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("merge of numeric-looking tiers preserves left order", {
  kn1 <- knowledge(tier(`1` ~ V1, `3` ~ V3))
  kn2 <- knowledge(tier(`1` ~ V4, `2` ~ V2, `3` ~ V5))

  kn12 <- kn1 + kn2

  expect_equal(
    kn12$tiers,
    tiers_tbl("1", "3", "2") # order: left first, new labels appended
  )

  ## duplicated variable: e1 wins because it’s listed first
  kn1a <- knowledge(tier(`1` ~ V1))
  kn2a <- knowledge(tier(`1` ~ V1_new))
  expect_equal(
    (kn1a + kn2a)$vars$var[1],
    "V1" # takes first definition from kn1a
  )
})

testthat::test_that("merge of arbitrary labels concatenates e1 order then new from e2", {
  kn1 <- knowledge(tier(A ~ V1, AA ~ V3))
  kn2 <- knowledge(tier(A ~ V4, B ~ V2))

  kn12 <- kn1 + kn2

  expect_equal(
    kn12$tiers,
    tiers_tbl("A", "AA", "B") # “B” appended after all of kn1’s labels
  )
})

testthat::test_that("merge errors if resulting tiers violate required-edge order", {
  kn_left <- knowledge(tier(One ~ V1))
  kn_right <- knowledge(
    tier(Two ~ V2),
    required(V2 ~ V1)
  )

  expect_error(
    kn_left + kn_right,
    "Edge(s) violate tier ordering",
    fixed = TRUE
  )
})

testthat::test_that("merge errors if required and forbidden edges overlap", {
  kn1 <- knowledge(
    forbidden(V1 ~ V2),
    forbidden(V2 ~ V3)
  )
  kn2 <- knowledge(
    required(V1 ~ V2),
    required(V2 ~ V3)
  )

  expect_error(
    kn1 + kn2,
    "Edge(s) appear as both forbidden and required: V1 --> V2, V2 --> V3",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# add_vars()
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("add_vars adds new vars and ignores existing ones (unfrozen)", {
  kn <- knowledge()
  kn <- add_vars(kn, c("A", "B"))
  expect_equal(
    kn$vars,
    tibble::tibble(var = c("A", "B"), tier = NA_character_)
  )

  kn2 <- add_vars(kn, c("A", "B")) # duplicates supplied again
  expect_equal(nrow(kn2$vars), 2L)
})

testthat::test_that("add_vars respects frozen knowledge objects", {
  kn_frozen <- knowledge(data.frame(A = 1, B = 2, check.names = FALSE))

  expect_silent(add_vars(kn_frozen, c("A"))) # existing var is OK
  expect_error(
    add_vars(kn_frozen, c("A", "C")), # new var should fail
    "Unknown variable(s): [C]",
    fixed = TRUE
  )
})

testthat::test_that("add_vars validates input types", {
  expect_error(add_vars("not_kn", c("X")), "knowledge")
  expect_error(add_vars(knowledge(), X))
})

# ──────────────────────────────────────────────────────────────────────────────
# forbidden and required
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("forbid_edge() and require_edge() add single edges", {
  kn <- knowledge()
  kn_f <- forbid_edge(kn, V1 ~ V2)
  expect_equal(kn_f$edges, tibble::tibble(
    status    = "forbidden",
    from      = "V1",
    to        = "V2",
    tier_from = NA_character_,
    tier_to   = NA_character_
  ))

  kn_r <- require_edge(kn, V1 ~ V2)
  expect_equal(kn_r$edges$status, "required")
  expect_equal(kn_r$edges$from, "V1")
  expect_equal(kn_r$edges$to, "V2")
})

testthat::test_that("forbid_edge() and require_edge() need two-sided formulas", {
  kn <- knowledge()
  expect_error(forbid_edge(kn), "needs at least one")
  expect_error(forbid_edge(kn, V1), "two-sided formula")
  expect_error(require_edge(kn, 1), "two-sided formula")
})

testthat::test_that("forbid_edge()/require_edge() respect tidy-select on either side", {
  kn <- knowledge(
    tier(
      T1 ~ Y,
      T2 ~ X1 + X2
    )
  )
  kn <- forbid_edge(
    kn,
    starts_with("X") ~ Y # X1 → Y, X2 → Y
  )
  expect_equal(
    dplyr::arrange(kn$edges, from)$from,
    c("X1", "X2")
  )
  expect_true(all(kn$edges$to == "Y"))

  kn2 <- require_edge(kn, Y ~ matches("^X[12]$"))
  expect_equal(sort(kn2$edges$status), c("forbidden", "forbidden", "required", "required"))
})

testthat::test_that("forbidden() and required() inside knowledge() create edges", {
  kn <- knowledge(
    tier(
      A ~ V1,
      B ~ V2,
      C ~ Y
    ),
    forbidden(starts_with("V") ~ Y),
    required(V1 ~ V2)
  )
  expect_equal(
    kn$edges$status,
    c("forbidden", "forbidden", "required")
  )
  expect_equal(
    kn$edges$from,
    c("V1", "V2", "V1")
  )
  expect_equal(
    kn$edges$to,
    c("Y", "Y", "V2")
  )
  kn <- knowledge(
    tier(
      A ~ V1,
      B ~ V2,
      C ~ V3
    ),
    forbidden(starts_with("V") ~ V3), # will not forbid self loop
    required(V1 ~ V2)
  )
  expect_equal(
    kn$edges$status,
    c("forbidden", "forbidden", "required")
  )
  expect_equal(
    kn$edges$from,
    c("V1", "V2", "V1")
  )
  expect_equal(
    kn$edges$to,
    c("V3", "V3", "V2")
  )
})

testthat::test_that("knowledge() errors on forbidden + required clash", {
  expect_error(
    knowledge(
      forbidden(V1 ~ V2),
      required(V1 ~ V2)
    ),
    "appear as both forbidden and required",
    fixed = TRUE
  )
})

testthat::test_that("knowledge() errors when required edges are bidirectional", {
  expect_error(
    knowledge(required(V1 ~ V2, V2 ~ V1)),
    "required in both directions",
    fixed = TRUE
  )
})

testthat::test_that("knowledge() rejects unknown top-level calls", {
  expect_error(
    knowledge(foo(V1)),
    "Only tier(), forbidden(), required(), and exogenous()",
    fixed = TRUE
  )
})

test_that("require_edge() errors when called with no formulas", {
  kn <- knowledge() # empty knowledge object

  expect_error(
    require_edge(kn), # no ... arguments
    "require_edge() needs at least one two-sided formula.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
#   forbid_tier_violations()
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("forbid_tier_violations() adds exactly the uphill edges", {
  kn <- knowledge(
    tier(
      1 ~ V1 + V2,
      2 ~ V3,
      3 ~ V4 + V5
    )
  )

  expect_equal(nrow(kn$edges), 0) # sanity: no edges yet

  kn2 <- forbid_tier_violations(kn)

  edges <- dplyr::filter(kn2$edges, status == "forbidden")

  # 8 total
  expect_equal(nrow(edges), 8)

  # every forbidden edge must go *downhill*
  expect_true(all(edges$tier_from > edges$tier_to))

  # spot–check one edge from each block
  expect_true(any(edges$from == "V4" & edges$to == "V1"))
  expect_true(any(edges$from == "V3" & edges$to == "V1"))
})

testthat::test_that("calling it again is a no-op (no duplicate edges)", {
  kn <- knowledge(tier(1 ~ V1, 2 ~ V2))
  kn1 <- forbid_tier_violations(kn)
  kn2 <- forbid_tier_violations(kn1)

  expect_equal(nrow(kn1$edges), nrow(kn2$edges))
})

testthat::test_that("single-tier or untiered variables add no edges", {
  # single tier
  kn_single <- knowledge(tier(1 ~ V1 + V2 + V3))
  kn_single <- forbid_tier_violations(kn_single)
  expect_equal(nrow(kn_single$edges), 0)

  # untiered variables
  df <- data.frame(V1 = 1, V2 = 1, V3 = 1)
  kn_mixed <- knowledge(df, tier(1 ~ V1 + V2)) # V3 has tier NA
  kn_mixed <- forbid_tier_violations(kn_mixed)
  expect_equal(nrow(kn_mixed$edges), 0) # NA tiers ignored
})

testthat::test_that("function errors on non-knowledge objects", {
  expect_error(forbid_tier_violations(list()), "knowledge")
})
# ──────────────────────────────────────────────────────────────────────────────
# as_tetrad_knowledge()
# ──────────────────────────────────────────────────────────────────────────────
test_that("as_tetrad_knowledge() errors when rJava is missing", {
  kn <- knowledge(tibble::tibble(A = 1), tier(1 ~ A))

  # run this branch only when rJava *isn't* installed
  skip_if(requireNamespace("rJava", quietly = TRUE), "rJava available; skip guard test")

  expect_error(
    as_tetrad_knowledge(kn),
    "Package 'rJava' is required for as_tetrad_knowledge\\(\\)\\.",
    perl = TRUE
  )
})

test_that("as_tetrad_knowledge() passes tiers and edges to the Java proxy", {
  kn <- knowledge(
    tibble::tibble(X = 1, Y = 2, Z = 3),
    tier(1 ~ X),
    tier(2 ~ Y + Z),
    forbidden(Y ~ X),
    required(X ~ Z)
  )

  fake <- rlang::env(
    tiers     = list(),
    forbidden = list(),
    required  = list()
  )

  new_stub <- function(class) {
    list(
      addToTier    = function(i, v) fake$tiers <- append(fake$tiers, list(c(i, v))),
      setForbidden = function(f, t) fake$forbidden <- append(fake$forbidden, list(c(f, t))),
      setRequired  = function(f, t) fake$required <- append(fake$required, list(c(f, t)))
    )
  }

  mockery::stub(
    as_tetrad_knowledge, "requireNamespace",
    function(pkg, quietly = FALSE) pkg == "rJava"
  )
  mockery::stub(as_tetrad_knowledge, "rJava::.jinit", function(...) NULL)
  mockery::stub(as_tetrad_knowledge, "rJava::.jnew", new_stub)

  j <- as_tetrad_knowledge(kn)
  expect_type(j, "list")

  expect_equal(
    fake$tiers |> purrr::map_chr(~ paste(.x, collapse = ":")),
    c("1:X", "2:Y", "2:Z")
  )
  expect_equal(
    fake$forbidden |> purrr::map_chr(~ paste(.x, collapse = ">")),
    "Y>X"
  )
  expect_equal(
    fake$required |> purrr::map_chr(~ paste(.x, collapse = ">")),
    "X>Z"
  )
})


# ──────────────────────────────────────────────────────────────────────────────
# as_bnlearn_knowledge()
# ──────────────────────────────────────────────────────────────────────────────

test_that("as_bnlearn_knowledge() returns correct whitelist and blacklist", {
  kn <- knowledge(
    tibble::tibble(A = 1, B = 2),
    tier(1 ~ A),
    tier(2 ~ B),
    required(A ~ B) # whitelist candidate
  )

  # tier rule makes B -> A a forbidden edge
  out <- as_bnlearn_knowledge(kn)

  expect_type(out, "list")
  expect_named(out, c("whitelist", "blacklist"))

  expect_equal(
    out$whitelist,
    data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
  )

  expect_true(
    any(out$blacklist$from == "B" & out$blacklist$to == "A")
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# as_pcalg_constraints()
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("errors if any tiers are present", {
  kn <- knowledge(tier(1 ~ X1 + X2))
  expect_error(
    as_pcalg_constraints(kn, labels = c("X1", "X2")),
    "Tiered background knowledge cannot be utilised"
  )
})

testthat::test_that("errors on asymmetric edges when directed_as_undirected = FALSE", {
  kn <- knowledge(
    data.frame(X1 = 1, X2 = 2),
    forbidden(X1 ~ X2) # only one direction
  )
  expect_error(
    as_pcalg_constraints(kn, labels = c("X1", "X2")),
    "no symmetrical counterpart"
  )
})

testthat::test_that("symmetrical counterpart edges when directed_as_undirected = TRUE", {
  kn <- knowledge(
    data.frame(X1 = 1, X2 = 2, Y = 3),
    forbidden(X1 ~ X2),
    required(Y ~ X1)
  )
  cons <- as_pcalg_constraints(
    kn,
    labels = c("X1", "X2", "Y"),
    directed_as_undirected = TRUE
  )
  # forbidden should be symmetric
  expect_true(cons$fixedGaps["X1", "X2"])
  expect_true(cons$fixedGaps["X2", "X1"])
  # required should be symmetric
  expect_true(cons$fixedEdges["Y", "X1"])
  expect_true(cons$fixedEdges["X1", "Y"])
})

testthat::test_that("works when forbidden edges are fully symmetric via DSL", {
  kn <- knowledge(
    data.frame(X1 = 1, X2 = 2, Y = 3),
    forbidden(X1 ~ X2, X2 ~ X1)
  )

  cons <- as_pcalg_constraints(
    kn,
    labels = c("X1", "X2", "Y")
  )

  # fixedGaps should have exactly the two symmetric entries
  expect_true(cons$fixedGaps["X1", "X2"])
  expect_true(cons$fixedGaps["X2", "X1"])
  # no other forbidden pairs
  expect_equal(sum(cons$fixedGaps), 2)

  # fixedEdges should be entirely FALSE
  expect_false(any(cons$fixedEdges))
})

testthat::test_that("result has correct dimnames and dimensions", {
  labels <- c("A", "B", "C", "D")
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3, D = 4),
    forbidden(A ~ B),
    required(C ~ D)
  )
  cons <- as_pcalg_constraints(kn, labels = labels, directed_as_undirected = TRUE)
  expect_equal(dim(cons$fixedGaps), c(4L, 4L))
  expect_equal(dimnames(cons$fixedGaps), list(labels, labels))
  expect_equal(dim(cons$fixedEdges), c(4L, 4L))
  expect_equal(dimnames(cons$fixedEdges), list(labels, labels))
})
testthat::test_that("create pcalg cons without providing labels", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3, D = 4),
    forbidden(A ~ B),
    required(C ~ D)
  )
  labels <- kn$vars$var
  expect_equal(
    as_pcalg_constraints(kn, directed_as_undirected = TRUE),
    as_pcalg_constraints(kn, labels = labels, directed_as_undirected = TRUE)
  )
})

testthat::test_that("labels errors are thrown for pcalg constraints conversion", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3, D = 4),
    forbidden(A ~ B),
    required(C ~ D)
  )
  labels <- NULL
  expect_error(
    as_pcalg_constraints(kn, labels = labels, directed_as_undirected = TRUE),
    "`labels` must be a non-empty character vector."
  )
  labels <- c("A", "A", "A", "A")
  expect_error(
    as_pcalg_constraints(kn, labels = labels, directed_as_undirected = TRUE),
    "labels` must be unique."
  )
  labels <- c("A", "B")
  expect_error(
    as_pcalg_constraints(kn, labels = labels, directed_as_undirected = TRUE),
    "The following is missing: \\[C, D\\]"
  )
  labels <- c("A", "B", "C", "D", "E")
  expect_error(
    as_pcalg_constraints(kn, labels = labels, directed_as_undirected = TRUE),
    "`labels` contained variables that were not in the knowledge object: [E]",
    fixed = TRUE
  )
})

test_that("as_pcalg_constraints() detects edges that reference unknown vars", {
  kn_forb <- knowledge(
    tibble::tibble(A = 1, B = 2),
    forbidden(A ~ B)
  )
  kn_forb$edges$to[1] <- "X" # X not in vars or labels

  expect_error(
    as_pcalg_constraints(kn_forb,
      labels = c("A", "B"),
      directed_as_undirected = TRUE
    ),
    "Forbidden edge refers to unknown variable",
    fixed = FALSE
  )

  kn_req <- knowledge(
    tibble::tibble(A = 1, B = 2),
    required(A ~ B)
  )
  kn_req$edges$to[1] <- "Y"

  expect_error(
    as_pcalg_constraints(kn_req,
      labels = c("A", "B"),
      directed_as_undirected = TRUE
    ),
    "Forbidden edge refers to unknown variable",
    fixed = FALSE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# exogenous() or exo()
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("exogenous() creates a variable that has all ingoing nodes forbidden", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(A)
  )

  expect_equal(kn$vars$var, c("A", "B", "C"))
  expect_equal(kn$edges$status, c("forbidden", "forbidden"))
  expect_equal(kn$edges$from, c("B", "C"))
  expect_equal(kn$edges$to, c("A", "A"))
})

testthat::test_that("exogenous() can take a list of variables", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(c(A, B))
  )

  expect_equal(kn$vars$var, c("A", "B", "C"))
  expect_equal(kn$edges$status, c("forbidden", "forbidden", "forbidden", "forbidden"))
  expect_equal(kn$edges$from, c("A", "B", "C", "C"))
  expect_equal(kn$edges$to, c("B", "A", "A", "B"))
})

testthat::test_that("exo() is an alias for exogenous()", {
  kn1 <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exo(A)
  )

  kn2 <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(A)
  )

  expect_equal(kn1, kn2)
})

testthat::test_that("exogenous() works with multiple variables specified in different ways", {
  df <- data.frame(X1 = 1, X11 = 2, Y = 3)
  kn1 <- knowledge(
    df,
    exo(c(X1, X11))
  )

  kn2 <- knowledge(
    df,
    exo(X1, X11)
  )

  kn3 <- knowledge(
    df,
    exo(starts_with("X"))
  )

  kn4 <- knowledge(
    df,
    exo(ends_with("1"))
  )
  expect_equal(kn1, kn2)
  expect_equal(kn1, kn3)
  expect_equal(kn1, kn4)
})

testthat::test_that("exogenous() with no variables errors", {
  expect_error(
    knowledge(
      data.frame(A = 1, B = 2, C = 3),
      exogenous()
    ),
    "exogenous() needs at least one variable specification.",
    fixed = TRUE
  )
})

testthat::test_that("exogenous() gives error for non-existent variables", {
  expect_error(
    knowledge(
      data.frame(A = 1, B = 2, C = 3),
      exogenous(D)
    ),
    "Unknown variable(s): [D]",
    fixed = TRUE
  )
})

testthat::test_that("exogenous() handles duplicate variables gracefully", {
  kn1 <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(c(A, A))
  )

  kn2 <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(A)
  )

  expect_equal(kn1, kn2)
})

testthat::test_that("multiple calls of exogenous() accumulate correctly", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(A),
    exogenous(B)
  )

  expect_equal(kn$edges$status, c("forbidden", "forbidden", "forbidden", "forbidden"))
  expect_setequal(kn$edges$from, c("B", "C", "A", "C"))
  expect_setequal(kn$edges$to, c("A", "A", "B", "B"))
})

testthat::test_that("exogenous() is invariant to order of variables", {
  kn1 <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(c(A, B))
  )

  kn2 <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    exogenous(c(B, A))
  )

  expect_equal(kn1, kn2)
})

test_that("exogenous() errors when it conflicts with required()", {
  expect_error(
    knowledge(
      data.frame(A = 1, B = 2),
      required(B ~ A),
      exogenous(A)
    ),
    "appear as both forbidden and required"
  )
})

test_that("exogenous() is idempotent", {
  kn1 <- knowledge(data.frame(A = 1, B = 2, C = 3), exogenous(A))
  kn2 <- knowledge(data.frame(A = 1, B = 2, C = 3), exogenous(c(A, A)))
  expect_equal(kn1, kn2)
})

test_that("exogenous() on unknown var errors when frozen", {
  kn <- knowledge(data.frame(A = 1, B = 2))
  expect_error(
    add_root(kn, C),
    "Unknown variable"
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# remove functions
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("remove_edges() drops forbidden and required edges", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3, D = 4),
    tier(1 ~ A + B, 2 ~ C, 3 ~ D),
    forbidden(A ~ C, B ~ D),
    required(B ~ C, C ~ D)
  )
  # check edges present
  testthat::expect_true(any(kn$edges$status == "forbidden" & kn$edges$from == "A" & kn$edges$to == "C"))
  testthat::expect_true(any(kn$edges$status == "required" & kn$edges$from == "C" & kn$edges$to == "D"))
  # remove a forbidden edge
  kn2 <- remove_edges(kn, A ~ C)
  testthat::expect_false(any(kn2$edges$from == "A" & kn2$edges$to == "C"))
  # remove a required edge
  kn3 <- remove_edges(kn, C ~ D)
  testthat::expect_false(any(kn3$edges$from == "C" & kn3$edges$to == "D"))
  # other edges remain
  testthat::expect_true(any(kn3$edges$from == "B" & kn3$edges$to == "D"))
})

test_that("remove_edges() errors when called with no formulas", {
  kn <- knowledge(
    tibble::tibble(A = 1, B = 2),
    forbidden(A ~ B)
  )

  expect_error(
    remove_edges(kn),
    "remove_edges() needs at least one two-sided formula.",
    fixed = TRUE
  )
})

testthat::test_that("remove_edges() supports multiple formulas and tidyselect", {
  kn <- knowledge(
    data.frame(X1 = 1, X2 = 2, Y = 3),
    tier(1 ~ X1 + X2, 2 ~ Y),
    forbidden(X1 ~ Y, X2 ~ Y),
    required(X2 ~ X1)
  )

  # remove both X1→Y and X2→Y in one call
  kn2 <- remove_edges(kn, X1 ~ Y, X2 ~ Y)
  testthat::expect_false(any(kn2$edges$from %in% c("X1", "X2") & kn2$edges$to == "Y"))

  # remove via character‐vector on the LHS
  kn3 <- remove_edges(kn, c("X1", "X2") ~ Y)
  testthat::expect_false(any(kn3$edges$to == "Y"))
})

testthat::test_that("remove_edges() warns if no edges matched", {
  kn <- knowledge(
    data.frame(A = 1, B = 2),
    forbidden(A ~ B)
  )
  testthat::expect_error(
    remove_edges(kn, B ~ C),
    "remove_edges() matched no edges",
    fixed = TRUE
  )
})

testthat::test_that("remove_vars() drops vars and associated edges", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    forbidden(A ~ B),
    required(B ~ C)
  )
  testthat::expect_true("B" %in% kn$vars$var)
  testthat::expect_true(any(kn$edges$to == "B" | kn$edges$from == "B"))
  kn2 <- remove_vars(kn, B)
  testthat::expect_false("B" %in% kn2$vars$var)
  testthat::expect_false(any(kn2$edges$from == "B" | kn2$edges$to == "B"))
})

testthat::test_that("remove_vars() accepts tidyselect and character vector", {
  kn <- knowledge(
    data.frame(foo = 1, bar = 2, baz = 3),
    forbidden(foo ~ bar),
    forbidden(bar ~ baz)
  )
  kn2 <- remove_vars(kn, starts_with("ba"))
  testthat::expect_false(any(grepl("^ba", kn2$vars$var)))
  kn3 <- remove_vars(kn, c("foo", "baz"))
  testthat::expect_false(any(kn3$vars$var %in% c("foo", "baz")))
})

testthat::test_that("remove_vars() errors on no matches", {
  kn <- knowledge(data.frame(A = 1, B = 2))
  testthat::expect_error(
    remove_vars(kn, X),
    "matched no variables"
  )
})

testthat::test_that("remove_tiers() drops tier and resets vars", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    tier("alpha" ~ A + B, "beta" ~ C)
  )
  testthat::expect_true("alpha" %in% kn$tiers$label)
  testthat::expect_equal(kn$vars$tier[kn$vars$var == "A"], "alpha")
  kn2 <- remove_tiers(kn, "alpha")
  testthat::expect_false("alpha" %in% kn2$tiers$label)
  testthat::expect_true(is.na(kn2$vars$tier[kn2$vars$var == "A"]))
})

testthat::test_that("remove_tiers() accepts numeric index", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    tier(1 ~ A, 2 ~ B, 3 ~ C)
  )
  # remove second tier (label "2")
  kn2 <- remove_tiers(kn, 2)
  testthat::expect_false("2" %in% kn2$tiers$label)
  testthat::expect_true(is.na(kn2$vars$tier[kn2$vars$var == "B"]))
})

testthat::test_that("remove_tiers() no-op if no match", {
  kn <- knowledge(
    data.frame(X = 1, Y = 2),
    tier("t1" ~ X, "t2" ~ Y)
  )
  kn2 <- remove_tiers(kn, "none")
  testthat::expect_identical(kn2, kn)
})

testthat::test_that("chaining remove_* works together", {
  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3, D = 4),
    tier(1 ~ A + B, 2 ~ C, 3 ~ D),
    forbidden(A ~ C),
    required(B ~ D)
  )
  kn2 <- kn |>
    remove_edges(A ~ C) |>
    remove_vars(D) |>
    remove_tiers(3)
  testthat::expect_false(any(kn2$edges$from == "A" & kn2$edges$to == "C"))
  testthat::expect_false("D" %in% kn2$vars$var)
  testthat::expect_false("3" %in% kn2$tiers$label)
})

# ──────────────────────────────────────────────────────────────────────────────
# deparse_knowledge()
# ──────────────────────────────────────────────────────────────────────────────


testthat::test_that("deparse_knowledge() emits minimal code for empty knowledge", {
  kn <- knowledge()
  code <- deparse_knowledge(kn)
  expected <- "knowledge(\n)"
  testthat::expect_equal(code, expected)
})

testthat::test_that("deparse_knowledge() includes data-frame name when provided", {
  df <- data.frame(A = 1, B = 2)
  kn <- knowledge(df, tier(1 ~ A, 2 ~ B))
  code <- deparse_knowledge(kn, "df")
  expected <- paste0(
    "knowledge(df,",
    "\n  tier(",
    "\n    1 ~ A,",
    "\n    2 ~ B",
    "\n  )",
    "\n)"
  )
  testthat::expect_equal(code, expected)
})

testthat::test_that("deparse_knowledge() groups multiple tiers into one tier() call", {
  df <- data.frame(X = 1, Y = 2, Z = 3)
  kn <- knowledge(
    df,
    tier(first ~ X + Y, second ~ Z)
  )
  code <- deparse_knowledge(kn, "df")
  testthat::expect_true(grepl("tier\\(\\s*first ~ X \\+ Y,\\s*second ~ Z\\s*\\)", code))
})

testthat::test_that("deparse_knowledge() collapses forbidden edges by source", {
  df <- data.frame(A = 1, B = 2, C = 3, D = 4)
  kn <- knowledge(
    df,
    forbidden(A ~ C, A ~ D, B ~ C)
  )
  code <- deparse_knowledge(kn, "df")
  # should have a single forbidden() call with two formulas:
  #   A ~ C + D
  #   B ~ C
  testthat::expect_true(
    grepl(
      "forbidden\\(\\s*A ~ C \\+ D,\\s*B ~ C\\s*\\)",
      code
    )
  )
})

testthat::test_that("deparse_knowledge() collapses required edges by source", {
  df <- data.frame(P = 1, Q = 2, R = 3)
  kn <- knowledge(
    df,
    required(P ~ Q, P ~ R)
  )
  code <- deparse_knowledge(kn, "df")
  testthat::expect_true(
    grepl("required\\(\\s*P ~ Q \\+ R\\s*\\)", code)
  )
})

testthat::test_that("deparse_knowledge() round-trips: eval(parse(code)) equals original", {
  df <- data.frame(A = 1, B = 2, C = 3)
  kn <- knowledge(
    df,
    tier(1 ~ A + B, 2 ~ C),
    forbidden(A ~ C),
    required(B ~ A)
  )
  code <- deparse_knowledge(kn, "df")
  kn2 <- eval(parse(text = code))
  testthat::expect_equal(kn2, kn)
})
