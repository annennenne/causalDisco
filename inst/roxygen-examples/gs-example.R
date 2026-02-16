data(tpc_example)

kn <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth")
)


# Recommended path using disco()
gs_bnlearn <- gs(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05
)
disco(tpc_example, gs_bnlearn, knowledge = kn)

# or using gs_bnlearn directly
gs_bnlearn <- gs_bnlearn |> set_knowledge(kn)
gs_bnlearn(tpc_example)


# With all algorithm arguments specified
gs_bnlearn <- gs(
  engine = "bnlearn",
  test = "fisher_z",
  alpha = 0.05,
  max.sx = 2,
  debug = FALSE,
  undirected = TRUE
)

disco(tpc_example, gs_bnlearn)
