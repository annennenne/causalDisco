### tpdag() example ###

data(tpcExample)
vnames <- names(tpcExample)

kn <- knowledge(
  tpcExample,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

ss_cor <- causalDisco:::make_suff_stat(tpcExample, type = "corTest")
wrapped_test <- causalDisco:::dirTest(causalDisco::corTest, vnames, kn)
pc_cons <- causalDisco:::.pcalg_constraints_from_knowledge(
  kn,
  labels = vnames,
  directed_as_undirected = TRUE
)

set.seed(1)
skel <- pcalg::skeleton(
  suffStat   = ss_cor,
  indepTest  = wrapped_test,
  alpha      = 0.05,
  labels     = vnames,
  method     = "stable.fast",
  fixedGaps  = pc_cons$fixedGaps,
  fixedEdges = pc_cons$fixedEdges
)

tp <- causalDisco:::tpdag(skel, knowledge = kn)
tp
