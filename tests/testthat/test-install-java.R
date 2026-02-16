test_that("get_os returns correct OS", {
  mock_sys <- mockery::mock(c(sysname = "Windows"))
  mockery::stub(get_os, "Sys.info", mock_sys)
  expect_equal(get_os(), "windows")

  mock_sys <- mockery::mock(c(sysname = "Darwin"))
  mockery::stub(get_os, "Sys.info", mock_sys)
  expect_equal(get_os(), "darwin")
})

test_that("detect_arch returns correct architecture", {
  mock_sys <- mockery::mock(c(machine = "amd64"))
  mockery::stub(detect_arch, "Sys.info", mock_sys)
  expect_equal(detect_arch(), "x64")

  mock_sys <- mockery::mock(c(machine = "aarch64"))
  mockery::stub(detect_arch, "Sys.info", mock_sys)
  expect_equal(detect_arch(), "aarch64")

  mock_sys <- mockery::mock(c(machine = "unknown"))
  mockery::stub(detect_arch, "Sys.info", mock_sys)
  expect_error(detect_arch(), "Unsupported architecture: unknown")
})

test_that("detect_jdk_folder picks longest folder name", {
  withr::with_tempdir({
    tmp <- getwd()

    dir1 <- file.path(tmp, "jdk-25")
    dir2 <- file.path(tmp, "jdk-25+123")

    dir.create(dir1)
    dir.create(dir2)

    result <- detect_jdk_folder(tmp)
    expect_equal(result, dir2)
  })
})

test_that("detect_jdk_folder returns NULL if no folders", {
  withr::with_tempdir({
    result <- detect_jdk_folder(getwd())
    expect_null(result)
  })
})


test_that("download URL is correct", {
  expect_equal(
    jdk_download_url("x64", "windows"),
    "https://api.adoptium.net/v3/binary/latest/25/ga/windows/x64/jdk/hotspot/normal/eclipse"
  )
})

test_that("install_java errors on unsupported OS", {
  mock_get_os <- mockery::mock("unknown")
  mockery::stub(install_java, "get_os", mock_get_os)

  expect_error(install_java(), "Unsupported OS: unknown")
})
