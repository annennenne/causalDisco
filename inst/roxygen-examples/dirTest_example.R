### dir_test() example ###

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

ss_cor <- causalDisco:::make_suff_stat(tpc_example, type = "cor_test")

wrapped_test <- causalDisco:::dir_test(pcalg::gaussCItest, vnames, kn)

ix_x <- match("child_x1", vnames)
ix_y <- match("youth_x3", vnames)
ix_S1 <- match("oldage_x5", vnames) # future-only S

wrapped_test(ix_x, ix_y, integer(0), ss_cor) # allowed
wrapped_test(ix_x, ix_y, ix_S1, ss_cor) # disallowed -> 0
