test_that("pcalg and causalDisco work with micd tests num data", {
  data(num_data)
  small_num_data <- num_data[1:200, ]
  n <- nrow(small_num_data)
  small_num_data$X1[sample(1:n, 20)] <- NA
  small_num_data$Z[sample(1:n, 20)] <- NA

  # With missing data
  pc_pcalg <- pc(engine = "pcalg", test = "fisher_z_twd")
  tpc_disco <- tpc(test = "fisher_z_twd")
  tfci_disco <- tfci(test = "fisher_z_twd")

  pc_mi <- disco(small_num_data, pc_pcalg)
  tpc_mi <- disco(small_num_data, tpc_disco)
  tfci_mi <- disco(small_num_data, tfci_disco)

  expect_equal(class(pc_mi), "Disco")
  expect_equal(class(tpc_mi), "Disco")
  expect_equal(class(tfci_mi), "Disco")

  # Imputation micd object
  num_data_imp <- mice::mice(small_num_data, m = 5, method = "pmm")

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


test_that("pcalg and causalDisco work with micd tests discrete data", {
  data(cat_data)
  small_cat_data <- cat_data[1:200, ]
  n <- nrow(small_cat_data)
  small_cat_data$X1[sample(1:n, 20)] <- NA
  small_cat_data$Z[sample(1:n, 20)] <- NA

  # With missing data
  pc_pcalg <- pc(engine = "pcalg", test = "g_square_twd")
  tpc_disco <- tpc(test = "g_square_twd")
  tfci_disco <- tfci(test = "g_square_twd")

  pc_mi <- disco(small_cat_data, pc_pcalg)
  tpc_mi <- disco(small_cat_data, tpc_disco)
  tfci_mi <- disco(small_cat_data, tfci_disco)

  expect_equal(class(pc_mi), "Disco")
  expect_equal(class(tpc_mi), "Disco")
  expect_equal(class(tfci_mi), "Disco")

  # Imputation micd object
  cat_data_imp <- mice::mice(small_cat_data, m = 5, method = "pmm")

  pc_pcalg <- pc(engine = "pcalg", test = "g_square_mi")
  tpc_disco <- tpc(test = "g_square_mi")
  tfci_disco <- tfci(test = "g_square_mi")

  pc_mi <- disco(cat_data_imp, pc_pcalg)
  tpc_mi <- disco(cat_data_imp, tpc_disco)
  tfci_mi <- disco(cat_data_imp, tfci_disco)

  expect_equal(class(pc_mi), "Disco")
  expect_equal(class(tpc_mi), "Disco")
  expect_equal(class(tfci_mi), "Disco")
})

test_that("pcalg and causalDisco work with micd tests mix data", {
  data(mix_data)
  small_mix_data <- mix_data[1:200, ]
  n <- nrow(small_mix_data)
  small_mix_data$X1[sample(1:n, 20)] <- NA
  small_mix_data$Z[sample(1:n, 20)] <- NA

  # With missing data
  pc_pcalg <- pc(engine = "pcalg", test = "conditional_gaussian_twd")
  tpc_disco <- tpc(test = "conditional_gaussian_twd")
  tfci_disco <- tfci(test = "conditional_gaussian_twd")

  pc_mi <- disco(small_mix_data, pc_pcalg)
  tpc_mi <- disco(small_mix_data, tpc_disco)
  tfci_mi <- disco(small_mix_data, tfci_disco)

  expect_equal(class(pc_mi), "Disco")
  expect_equal(class(tpc_mi), "Disco")
  expect_equal(class(tfci_mi), "Disco")

  # Imputation micd object
  mix_data_imp <- mice::mice(small_mix_data, m = 5, method = "pmm")

  pc_pcalg <- pc(engine = "pcalg", test = "conditional_gaussian_mi")
  tpc_disco <- tpc(test = "conditional_gaussian_mi")
  tfci_disco <- tfci(test = "conditional_gaussian_mi")

  pc_mi <- disco(mix_data_imp, pc_pcalg)
  tpc_mi <- disco(mix_data_imp, tpc_disco)
  tfci_mi <- disco(mix_data_imp, tfci_disco)

  expect_equal(class(pc_mi), "Disco")
  expect_equal(class(tpc_mi), "Disco")
  expect_equal(class(tfci_mi), "Disco")
})
