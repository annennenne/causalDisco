devtools::load_all()

# ──────────────────────────────────────────────────────────────────────────────

# EXAMPLE 1 ── Pure DSL (numeric tiers) ----------------------------------------
kn1 <-
  knowledge(
    tier(1 ~ V1 + V2, 2 ~ V3),
    forbidden(V1 ~ V3, edge_type = "undirected"),
    required(V1 ~ V2, V2 ~ V3)
  )

print(kn1)

# EXAMPLE 2 ── Verb pipeline with |> -------------------------------------------
kn2 <-
  knowledge() |>
  add_vars(c("A", "B", "C", "D")) |>
  add_tier(1, c("A", "B")) |>
  add_tier(2, "C") |>
  forbid_edge(A, C, edge_type = "undirected") |>
  require_edge(A, B)

print(kn2)

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
  add_tier(3, V7) # add third tier later

print(kn4)

# EXAMPLE 5 ── Combining objects with + ----------------------------------------
kn_combined <- kn1 + kn2
print(kn_combined)

# EXAMPLE 6 ── Convert to Tetrad Java object -----------------------------------
if (requireNamespace("rJava", quietly = TRUE)) {
  jk <- as_tetrad_knowledge(kn1)
  print(jk) # Java reference; prints class name & pointer
}

# EXAMPLE 7 ── Using tidyselect helpers ----------------------------------------
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
    tier(baby ~ V1 + V2, old ~ V3),
    required(V1 ~ V2)
  )
print(kn_custom_tier)

# ──────────────────────────────────────────────────────────────────────────────

# Make errors happen -----------------------------------------------------------
# tier violation
knowledge(
  tier(1 ~ V1 + V2, 2 ~ V3),
  required(V3 ~ V1)
) |> try()

# require edge
kn5 <- knowledge(
  required(V3 ~ V1)
)

# set tiers in another objects
kn6 <- knowledge(
  tier(1 ~ V1 + V2, 2 ~ V3)
)

# tier + knowledge violation
kn5 + kn6 |> try()

# set required and forbidden for same edge
knowledge(
  required(V1 ~ V2),
  forbidden(V1 ~ V2)
) |> try()

# make it happen another way
kn7 <- knowledge(
  forbidden(V3 ~ V1)
)
kn5 + kn7 |> try()

# make tier names conflict
kn8 <- knowledge(
  tier(baby ~ V1 + V2, old ~ V3)
)

kn9 <- knowledge(
  tier(baby ~ V1 + V2, young ~ V4, old ~ V3)
)

kn8 + kn9 |> try()
