data(tpc_example)

ss_cor <- make_suffStat(tpc_example, type = "cor_test")
ss_cor

ss_reg <- make_suffStat(tpc_example, type = "reg_test")
ss_reg
