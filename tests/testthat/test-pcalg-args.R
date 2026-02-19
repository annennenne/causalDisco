test_that("pcalg test argument works", {
  data(num_data)
  pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
  pc_result_pcalg <- disco(num_data, method = pc_pcalg)
  expect_equal(class(pc_result_pcalg), "Disco")

  fci_pcalg <- fci(engine = "pcalg", test = "fisher_z", alpha = 0.05)
  fci_result_pcalg <- disco(num_data, method = fci_pcalg)
  expect_equal(class(fci_result_pcalg), "Disco")

  data(cat_data)
  pc_pcalg_disc <- pc(engine = "pcalg", test = "g_square", alpha = 0.05)
  pc_result_pcalg_disc <- suppressWarnings(disco(
    cat_data,
    method = pc_pcalg_disc
  )) # Warnings too low sample size
  expect_equal(
    class(pc_result_pcalg_disc),
    "Disco"
  )

  fci_pcalg_disc <- fci(engine = "pcalg", test = "g_square", alpha = 0.05)
  fci_result_pcalg_disc <- suppressWarnings(disco(
    cat_data,
    method = fci_pcalg_disc
  )) # Warnings too low sample size
  expect_equal(
    class(fci_result_pcalg_disc),
    "Disco"
  )
})


test_that("pcalg score argument works", {
  data(num_data)
  ges_pcalg <- ges(engine = "pcalg", score = "sem_bic")
  ges_result_pcalg <- disco(num_data, method = ges_pcalg)
  expect_equal(class(ges_result_pcalg), "Disco")

  ges_pcalg <- ges(engine = "pcalg", score = "sem_bic_int")
  ges_result_pcalg <- disco(num_data, method = ges_pcalg)
  expect_equal(class(ges_result_pcalg), "Disco")
})
