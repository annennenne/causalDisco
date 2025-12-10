test_that("find_tetrad_jar stops when directory is missing or empty", {
  # Mock nzchar(dir) = FALSE OR dir.exists(dir) = FALSE
  m_nzchar <- mockery::mock(FALSE)
  m_dir_exists <- mockery::mock(FALSE)

  mockery::stub(find_tetrad_jar, "nzchar", m_nzchar)
  mockery::stub(find_tetrad_jar, "dir.exists", m_dir_exists)

  expect_error(
    find_tetrad_jar("1.0", dir = ""),
    regexp = "Tetrad directory not found"
  )
})

test_that("find_tetrad_jar warns and returns empty when JAR is missing", {
  fake_dir <- "/tmp/fake_tetrad"

  m_nzchar <- mockery::mock(TRUE)
  m_dir_exists <- mockery::mock(TRUE)
  m_file_exists <- mockery::mock(FALSE)

  mockery::stub(find_tetrad_jar, "nzchar", m_nzchar)
  mockery::stub(find_tetrad_jar, "dir.exists", m_dir_exists)
  mockery::stub(find_tetrad_jar, "file.exists", m_file_exists)

  expect_warning(
    out <- find_tetrad_jar("1.0", dir = fake_dir),
    regexp = "Tetrad GUI launcher JAR not found"
  )
  expect_identical(out, character(0))
})

test_that("find_tetrad_jar returns correct JAR path when file exists", {
  fake_dir <- "/tmp/fake_tetrad"
  version <- "1.2"

  m_nzchar <- mockery::mock(TRUE)
  m_dir_exists <- mockery::mock(TRUE)
  m_file_exists <- mockery::mock(TRUE)

  mockery::stub(find_tetrad_jar, "nzchar", m_nzchar)
  mockery::stub(find_tetrad_jar, "dir.exists", m_dir_exists)
  mockery::stub(find_tetrad_jar, "file.exists", m_file_exists)

  jar_name <- paste0("tetrad-gui-", version, "-launch.jar")
  expected <- file.path(fake_dir, jar_name)

  out <- find_tetrad_jar(version, dir = fake_dir)
  expect_identical(out, expected)
})
