### make_suff_stat() example ###

data(tpcExample)

ss_cor <- causalDisco:::make_suff_stat(tpcExample, type = "corTest")
ss_cor

ss_reg <- causalDisco:::make_suff_stat(tpcExample, type = "regTest")
ss_reg
