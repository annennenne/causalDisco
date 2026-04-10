test_that("pcalg and causalDisco work with mice objects with appropiate tests", {
  data(num_data)

  n <- nrow(num_data)
  num_data$X1[sample(1:n, 100)] <- NA
  num_data$Z[sample(1:n, 100)] <- NA

  num_data_imp <- mice::mice(num_data, m = 5, method = "pmm")

  pc_pcalg <- pc(engine = "pcalg", test = "fisher_z_mi")
  tpc_disco <- tpc(test = "fisher_z_mi")
  tfci_disco <- tfci(test = "fisher_z_mi")

  pc_mi <- disco(num_data_imp, pc_pcalg)
  tpc_mi <- disco(num_data_imp, tpc_disco)
  tfci_mi <- disco(num_data_imp, tfci_disco)

  expect_equal(class(pc_mi), "Disco")
  expect_equal(class(tpc_mi), "Disco")
  expect_equal(class(tfci_mi), "Disco")
})
