devtools::load_all()

# ──────────────────────────────────────────────────────────────────────────────

# EXAMPLE 1 ── Pure DSL (numeric tiers) ----------------------------------------
kn1 <-
  knowledge(
    tier(
      1 ~ V1 + V2,
      2 ~ V3,
      3 ~ c(V4, V5)
    ),
    forbidden(V1 ~ V3),
    required(V1 ~ V2, V2 ~ V3)
  )

print(kn1)

# EXAMPLE 2 ── Verb pipeline with |> -------------------------------------------
kn2 <-
  knowledge() |>
  add_vars(c("A", "B", "C", "D")) |>
  add_tier(1) |>
  add_to_tier(1 ~ A + B) |>
  add_tier(2) |>
  add_to_tier(2 ~ C) |>
  forbid_edge(A, C) |>
  require_edge(A ~ B)

print(kn2)

kn2.5 <- knowledge() |>
  add_tier(1) |>
  add_to_tier(1 ~ A + B)
kn2.5 <- add_tier(kn2.5, exposure, after = 1) |> add_to_tier(exposure ~ E1)
kn2.5 <- add_tier(kn2.5, outcome, after = exposure)
kn2.5 <- add_to_tier(kn2.5, outcome ~ O1)
kn2.5 <- add_tier(kn2.5, mediator, before = outcome) |> add_to_tier(mediator ~ M1)
print(kn2.5)

# EXAMPLE 3 ── DSL + data-frame seeding ----------------------------------------
df <- data.frame(X1 = 1, X2 = 2, X3 = 3, check.names = FALSE)

kn3 <-
  knowledge(
    df,
    tier(1 ~ X1, 2 ~ X2 + X3),
    required(X1 ~ X2)
  )

print(kn3)

# EXAMPLE 4 ── Mix DSL start + verb refinement ---------------------------------
kn4 <-
  knowledge(
    tier(1 ~ V5, 2 ~ V6),
    forbidden(V5 ~ V6)
  ) |>
  add_tier(3) |>
  add_to_tier(3 ~ V7) # add third tier later

print(kn4)

# EXAMPLE 5 ── Combining objects with + ----------------------------------------
kn_combined <- kn1 + kn2
print(kn_combined)

# EXAMPLE 6 ── Convert to Tetrad Java object -----------------------------------
if (requireNamespace("rJava", quietly = TRUE)) {
  jk <- as_tetrad_knowledge(kn1)
  print(jk)
}

# EXAMPLE 7 ── Using tidyselect helpers ----------------------------------------
df <- data.frame(X1 = 1, X2 = 2, X3 = 3, check.names = FALSE)

kn_select_helpers <-
  knowledge(
    df,
    tier(1 ~ matches("^X[12]$"), 2 ~ ends_with("3")),
    required(X2 ~ X3)
  )
print(kn_select_helpers)

# EXAMPLE 8 ── Custom tier naming ----------------------------------------------
kn_custom_tier <-
  knowledge(
    tier(baby ~ V1 + V2, old ~ V3, adult ~ V4),
    required(V1 ~ V2)
  )
print(kn_custom_tier)

# EXAMPLE 9 ── Unfreezing ------------------------------------------------------
df <- data.frame(X1 = 1, X2 = 2, X3 = 3, check.names = FALSE)

kn_freezed <- knowledge(
  df,
  tier(1 ~ X1, 2 ~ X2 + X3),
  required(X1 ~ X2)
)
print("kn_freezed$frozen:")
print(kn_freezed$frozen)

kn_freezed <- unfreeze(kn_freezed)

print("kn_freezed$frozen:")
print(kn_freezed$frozen)

kn_freezed <- kn_freezed |>
  add_tier(3) |>
  add_to_tier(3 ~ V4) # does not throw error
print(kn_freezed)

# EXAMPLE 10 ── seq_tiers ------------------------------------------------------
# This example shows how to use seq_tiers to create a sequence of tiers.
# This can be used if you have many variables that follow a pattern.

df <- as.data.frame(
  matrix(runif(100), # 100 random numbers in (0,1)
    nrow = 1,
    ncol = 100,
    byrow = TRUE
  )
)

names(df) <- paste0("X_", 1:100) # label the columns X_1 … X_100

kn_seq_tiers <- knowledge(
  df,
  tier(
    seq_tiers(
      1:100,
      ends_with("_{i}")
    )
  ),
  required(X_1 ~ X_2)
)
print(kn_seq_tiers)

df <- data.frame(
  X_1 = 1,
  X_2 = 2,
  tier3_A = 3,
  Y5_ok = 4,
  check.names = FALSE
)

kn_seq_tiers2 <- knowledge(
  df,
  tier(
    seq_tiers(1:2, ends_with("_{i}")), # X_1, X_2
    seq_tiers(3, starts_with("tier{i}")), # tier3_…
    seq_tiers(5, matches("Y{i}_ok")) # exact match
  )
)

print(kn_seq_tiers2)

# EXAMPLE 11 ── Giving tiers with a numeric vector -----------------------------
df <- as.data.frame(
  matrix(runif(100), # 100 random numbers in (0,1)
    nrow = 1,
    ncol = 100,
    byrow = TRUE
  )
)

names(df) <- paste0("X_", 1:100) # label the columns X_1 … X_100

kn_numeric_tiers <-
  knowledge(
    df,
    tier(
      1:100
    )
  )
print(kn_numeric_tiers)

# EXAMPLE 12 ── Forbid tier violations ----------------------
df <- as.data.frame(
  matrix(runif(10), # 100 random numbers in (0,1)
    nrow = 1,
    ncol = 10,
    byrow = TRUE
  )
)

names(df) <- paste0("X_", 1:10) # label the columns X_1 … X_100

kn <-
  knowledge(
    df,
    tier(
      1:10
    )
  )
forbid_tier_violations(kn)

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Make error happen ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# tier violation
try(
  knowledge(
    tier(1 ~ V1 + V2, 2 ~ V3),
    required(V3 ~ V1)
  )
)

# require edge
kn5 <- knowledge(
  required(V3 ~ V1)
)

# set tiers in another objects
kn6 <- knowledge(
  tier(1 ~ V1 + V2, 2 ~ V3)
)

# tier + knowledge violation
try(kn5 + kn6)

# set required and forbidden for same edge
knowledge(
  required(V1 ~ V2),
  forbidden(V1 ~ V2)
) |> try()

# make it happen another way
kn7 <- knowledge(
  forbidden(V3 ~ V1)
)

try(kn5 + kn7)

# make tier names conflict
kn8 <- knowledge(
  tier(baby ~ V1 + V2, old ~ V3)
)

kn9 <- knowledge(
  tier(baby ~ V1 + V2, young ~ V4, old ~ V3)
)

try(kn8 + kn9)

# make name error
df <- data.frame(X1 = 1, X2 = 2, X3 = 3, check.names = FALSE)

knowledge(
  df,
  tier(baby ~ V1 + V2, old ~ V3),
  required(V1 ~ V4)
) |> try()

# Tier conflict
try(
  knowledge(
    tier(baby ~ V1 + V2, old ~ V3)
  ) |> add_to_tier(old ~ V4, after = old)
)

# Provide tiers with numeric vector without having variables
knowledge(
  tier(
    1:5
  ),
  required(V1 ~ V2)
) |> try()
