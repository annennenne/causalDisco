data(tpc_example)

ss_cor <- make_suff_stat(tpc_example, type = "cor_test")
ss_cor

ss_reg <- make_suff_stat(tpc_example, type = "reg_test")
ss_reg
