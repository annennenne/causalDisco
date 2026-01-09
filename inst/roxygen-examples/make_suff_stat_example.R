data(tpc_example)

ss_cor <- causalDisco:::make_suffStat(tpc_example, type = "cor_test")
ss_cor

ss_reg <- causalDisco:::make_suffStat(tpc_example, type = "reg_test")
ss_reg
