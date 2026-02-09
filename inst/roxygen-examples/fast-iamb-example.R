data(tpc_example)

kn <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth")
)


# Recommended path using disco()
fast_iamb_bnlearn <- fast_iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, fast_iamb_bnlearn, knowledge = kn)

# or using fast_iamb_bnlearn directly
fast_iamb_bnlearn <- fast_iamb_bnlearn |> set_knowledge(kn)
fast_iamb_bnlearn(tpc_example)


# With all algorithm arguments specified
fast_iamb_bnlearn <- fast_iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05,
  max.sx = 2,
  debug = FALSE,
  undirected = TRUE
)

disco(tpc_example, fast_iamb_bnlearn)
