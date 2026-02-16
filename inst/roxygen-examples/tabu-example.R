data(tpc_example)

# Recommended path using disco()
tabu_bnlearn <- tabu(engine = "bnlearn", score = "aic_g")
disco(tpc_example, tabu_bnlearn)

# or using tabu_bnlearn directly
tabu_bnlearn(tpc_example)

#### With tier knowledge ####
kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

tabu_bnlearn <- tabu(engine = "bnlearn", score = "bge")
disco(tpc_example, tabu_bnlearn, knowledge = kn)

# or using tabu_bnlearn directly
tabu_bnlearn <- tabu_bnlearn |> set_knowledge(kn)
tabu_bnlearn(tpc_example)

# With all algorithm arguments specified
tabu_bnlearn <- tabu(
  engine = "bnlearn",
  score = "loglik_g",
  debug = FALSE,
  tabu = 10,
  max.tabu = 10,
  max.iter = Inf,
  maxp = Inf,
  optimized = TRUE
)
disco(tpc_example, tabu_bnlearn)
