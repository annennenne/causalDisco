data(tpc_example)

# Recommended path using disco()
hc_bnlearn <- hc(engine = "bnlearn", score = "aic_g")
disco(tpc_example, hc_bnlearn)

# or using hc_bnlearn directly
hc_bnlearn(tpc_example)

#### With tier knowledge ####
kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

hc_bnlearn <- hc(engine = "bnlearn", score = "bge")
disco(tpc_example, hc_bnlearn, knowledge = kn)

# or using hc_bnlearn directly
hc_bnlearn <- hc_bnlearn |> set_knowledge(kn)
hc_bnlearn(tpc_example)

# With all algorithm arguments specified
hc_bnlearn <- hc(
  engine = "bnlearn",
  score = "loglik_g",
  debug = FALSE,
  restart = 10,
  perturb = 1,
  max.iter = Inf,
  maxp = Inf,
  optimized = TRUE
)
disco(tpc_example, hc_bnlearn)
