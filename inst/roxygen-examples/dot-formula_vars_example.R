# extract variable names from a RHS using tidyselect or symbols
data(tpc_example)
kn <- knowledge(tpc_example)

# tidyselect RHS
rhs1 <- rlang::expr(starts_with("youth"))
causalDisco:::.formula_vars(kn, rhs1)

# symbol arithmetic-style RHS (falls back to symbols)
rhs2 <- rlang::expr(child_x1 + youth_x3)
causalDisco:::.formula_vars(kn, rhs2)
