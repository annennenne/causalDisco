test_that("detect_arch returns x64 for amd64", {
  mock_sys <- mockery::mock(c(machine = "amd64"))
  mockery::stub(detect_arch, "Sys.info", mock_sys)

  expect_equal(detect_arch(), "x64")
})

test_that("detect_jdk_folder picks longest folder name", {
  tmp <- tempdir()
  dir1 <- file.path(tmp, "jdk-25")
  dir2 <- file.path(tmp, "jdk-25+123")
  dir.create(dir1)
  dir.create(dir2)

  result <- detect_jdk_folder(tmp)
  expect_equal(result, dir2)
})

test_that("download URL is correct", {
  expect_equal(
    jdk_download_url("x64", "windows"),
    "https://api.adoptium.net/v3/binary/latest/25/ga/windows/x64/jdk/hotspot/normal/eclipse"
  )
})
