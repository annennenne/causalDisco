test_that("pcalg test argument works", {
  # Fisher Z test with pcalg engine
  data("tpc_example")
  pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
  pc_result_pcalg <- disco(tpc_example, method = pc_pcalg)
  expect_equal(class(pc_result_pcalg), c("knowledgeable_caugi", "knowledge"))

  fci_pcalg <- fci(engine = "pcalg", test = "fisher_z", alpha = 0.05)
  fci_result_pcalg <- disco(tpc_example, method = fci_pcalg)
  expect_equal(class(fci_result_pcalg), c("knowledgeable_caugi", "knowledge"))

  # G-square test with pcalg engine
  disc_test_data <- make_disc_test_data(n = 2000)
  pc_pcalg_disc <- pc(engine = "pcalg", test = "g_square", alpha = 0.05)
  pc_result_pcalg_disc <- disco(disc_test_data, method = pc_pcalg_disc)
  expect_equal(
    class(pc_result_pcalg_disc),
    c("knowledgeable_caugi", "knowledge")
  )

  fci_pcalg_disc <- fci(engine = "pcalg", test = "g_square", alpha = 0.05)
  fci_result_pcalg_disc <- disco(disc_test_data, method = fci_pcalg_disc)
  expect_equal(
    class(fci_result_pcalg_disc),
    c("knowledgeable_caugi", "knowledge")
  )
})


test_that("pcalg score argument works", {
  data("tpc_example")
  ges_pcalg <- ges(engine = "pcalg", score = "sem_bic")
  ges_result_pcalg <- disco(tpc_example, method = ges_pcalg)
  expect_equal(class(ges_result_pcalg), c("knowledgeable_caugi", "knowledge"))

  ges_pcalg <- ges(engine = "pcalg", score = "sem_bic_int")
  ges_result_pcalg <- disco(tpc_example, method = ges_pcalg)
  expect_equal(class(ges_result_pcalg), c("knowledgeable_caugi", "knowledge"))
})
