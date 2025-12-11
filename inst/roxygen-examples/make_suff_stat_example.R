### make_suff_stat() example ###

data(tpc_example)

ss_cor <- causalDisco:::make_suff_stat(tpc_example, type = "cor_test")
ss_cor

ss_reg <- causalDisco:::make_suff_stat(tpc_example, type = "reg_test")
ss_reg
