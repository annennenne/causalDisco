### knowledge verbs example ###

data(tpc_example)

# create knowledge object using verbs
kn1 <- knowledge() |>
  add_vars(names(tpc_example)) |>
  add_tier(child) |>
  add_tier(old, after = child) |>
  add_tier(youth, before = old) |>
  add_to_tier(child ~ starts_with("child")) |>
  add_to_tier(youth ~ starts_with("youth")) |>
  add_to_tier(old ~ starts_with("oldage")) |>
  require_edge(child_x1 ~ youth_x3) |>
  forbid_edge(child_x2 ~ youth_x4) |>
  add_exogenous(child_x1) # synonyms: add_exo(), add_root()

# set kn1 to frozen
# (meaning you cannot add variables to the knowledge object anymore)
# this is to get a true on the identical check
kn1$frozen <- TRUE

# create identical knowledge object using DSL
kn2 <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("oldage")
  ),
  required(child_x1 ~ youth_x3),
  forbidden(child_x2 ~ youth_x4),
  exo(child_x1) # synonyms: exogenous(), root()
)

print(identical(kn1, kn2))

# cannot require an edge against tier direction
try(
  kn1 |> require_edge(oldage_x6 ~ child_x1)
)

# cannot forbid and require same edge
try(
  kn1 |> forbid_edge(child_x1 ~ youth_x3)
)
