data(tpc_example)

kn <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth")
)

##### iamb #####

# Recommended path using disco()
iamb_bnlearn <- iamb(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
disco(tpc_example, iamb_bnlearn, knowledge = kn)

# or using iamb_bnlearn directly
iamb_bnlearn <- iamb_bnlearn |> set_knowledge(kn)
iamb_bnlearn(tpc_example)


# With all algorithm arguments specified
iamb_bnlearn <- iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05,
  max.sx = 2,
  debug = FALSE,
  undirected = TRUE
)

disco(tpc_example, iamb_bnlearn)

##### iamb_fdr #####

iamb_fdr_bnlearn <- iamb_fdr(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, iamb_fdr_bnlearn, knowledge = kn)

##### fast_iamb #####

fast_iamb_bnlearn <- fast_iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, fast_iamb_bnlearn, knowledge = kn)

#### inter_iamb #####

inter_iamb_bnlearn <- inter_iamb(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, inter_iamb_bnlearn, knowledge = kn)
