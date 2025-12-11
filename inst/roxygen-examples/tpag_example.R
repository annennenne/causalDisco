### tpdag() example ###

# Full pipeline to a PAG using temporal rules
set.seed(3)
n <- 200
X1 <- rnorm(n)
X2 <- X1 + rnorm(n)
X3 <- X1 + rnorm(n)
X4 <- X3 + rnorm(n)

d <- data.frame(
  p1_X1 = X1,
  p1_X2 = X2,
  p2_X3 = X3,
  p2_X4 = X4
)
vnames <- names(d)

kn <- knowledge(
  d,
  tier(
    p1 ~ tidyselect::starts_with("p1_"),
    p2 ~ tidyselect::starts_with("p2_")
  )
)

ss_cor <- causalDisco:::make_suff_stat(d, type = "cor_test")
wrapped <- causalDisco:::dir_test(causalDisco::cor_test, vnames, kn)
cons <- causalDisco:::.pcalg_constraints_from_knowledge(
  kn,
  labels = vnames,
  directed_as_undirected = TRUE
)

skel <- pcalg::skeleton(
  suffStat   = ss_cor,
  indepTest  = wrapped,
  alpha      = 0.05,
  labels     = vnames,
  method     = "stable.fast",
  fixedGaps  = cons$fixedGaps,
  fixedEdges = cons$fixedEdges
)

fci_skel <- pcalg::pdsep(
  skel = skel,
  suffStat = ss_cor,
  indepTest = wrapped,
  p = length(vnames),
  sepset = skel@sepset,
  pMax = skel@pMax,
  unfVect = c(),
  alpha = 0.05
)

# Orient to a PAG (matrix with codes 0/1/2/3)
pag <- causalDisco:::tpag(fci_skel, knowledge = kn, unfVect = NULL, cautious = TRUE)
pag
