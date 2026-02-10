data(tpc_example)
vnames <- names(tpc_example)

kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

ss_cor <- causalDisco:::make_suffStat(tpc_example, type = "cor_test")
wrapped_test <- causalDisco:::dir_test(causalDisco::cor_test, vnames, kn)
pc_cons <- causalDisco:::.pcalg_constraints_from_knowledge(
  kn,
  labels = vnames,
  directed_as_undirected = TRUE
)

set.seed(1405)
skel <- pcalg::skeleton(
  suffStat = ss_cor,
  indepTest = wrapped_test,
  alpha = 0.05,
  labels = vnames,
  method = "stable.fast",
  fixedGaps = pc_cons$fixed_gaps,
  fixedEdges = pc_cons$fixed_edges
)
