### knowledge() example ###

# build knowledge from a data frame and a few DSL calls
data(tpcExample)
df <- head(tpcExample)

# knowledge objects are made with the knowledge() function
kn <- knowledge()


# knowledge objects contain tier information, forbidden and required edges
kn <- knowledge(
  tier(
    1 ~ V1 + V2,
    2 ~ V3
  ),
  required(V1 ~ V2),
  forbidden(V3 ~ V1)
)

# if a data frame is provided, variable names are checked against it
kn <- knowledge(
  df,
  tier(
    1 ~ child_x1 + child_x2,
    2 ~ youth_x3 + youth_x4,
    3 ~ oldage_x5 + oldage_x6
  )
)

# throws error
try(
  knowledge(
    df,
    tier(
      1 ~ child_x1 + child_x2,
      2 ~ youth_x3 + youth_x4,
      3 ~ oldage_x5 + woops
    ) # wrong name
  )
)

# using tidyselect helpers
kn <- knowledge(
  df,
  tier(
    1 ~ starts_with("child"), # can use tidyselect helpers
    2 ~ youth_x3 + youth_x4, # do not need quotes for tiers or variables
    3 ~ starts_with("oldage")
  ) # doesn't have to match data naming
)

# custom tier naming
kn <- knowledge(
  df,
  tier(
    "child" ~ starts_with("child"), # can use tidyselect helpers
    youth ~ starts_with("youth"), # do not need quotes for tiers
    elderly ~ starts_with("oldage")
  ) # doesn't have to match data naming
)

# There is also required and forbidden edges, which are specified like so
kn <- knowledge(
  df,
  required(child_x1 ~ youth_x3),
  forbidden(oldage_x6 ~ child_x1)
)

# You can also add exogenous variables
kn <- knowledge(
  df,
  exogenous(child_x1),
  exo(child_x2), # shorthand
  root(youth_x3) # another shorthand
)

# You can also build knowledge with a verb pipeline
kn <-
  knowledge() |>
  add_vars(c("A", "B", "C", "D")) |> # knowledge now only takes these variables
  add_tier(One) |>
  add_to_tier("One" ~ A + B) |>
  add_tier(2, after = One) |>
  add_to_tier(2 ~ C + D) |>
  forbid_edge("A" ~ C) |>
  require_edge(A ~ B)

# Mix DSL start + verb refinement
kn <-
  knowledge(
    tier(1 ~ V5, 2 ~ V6),
    forbidden(V5 ~ V6)
  ) |>
  add_tier(3, after = "2") |>
  add_to_tier(3 ~ V7) |> # add third tier later
  add_root(V1) |> # three ways to add roots
  add_exo(V2) |>
  add_exogenous(V3)

# Using seq_tiers for larger datasets
df <- as.data.frame(
  matrix(runif(100), # 100 random numbers in (0,1)
    nrow = 1,
    ncol = 100,
    byrow = TRUE
  )
)

names(df) <- paste0("X_", 1:100) # label the columns X_1,..., X_100

kn <- knowledge(
  df,
  tier(
    seq_tiers(
      1:100,
      ends_with("_{i}")
    )
  ),
  required(X_1 ~ X_2)
)

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
    seq_tiers(3, starts_with("tier{i}")), # tier3_
    seq_tiers(5, matches("Y{i}_ok")) # exact match
  )
)
