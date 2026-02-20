test_that("get_TETRAD_DIR returns option TETRAD_DIR when set", {
  old_opt <- getOption("TETRAD_DIR")
  on.exit(options(TETRAD_DIR = old_opt), add = TRUE)

  options(TETRAD_DIR = "/opt/tetrad")

  expect_equal(get_TETRAD_DIR(), "/opt/tetrad")
})

test_that("get_TETRAD_DIR returns TETRAD_DIR env var when option is NULL and dir exists", {
  # Ensure option is NULL
  old_opt <- getOption("TETRAD_DIR")
  on.exit(options(TETRAD_DIR = old_opt), add = TRUE)
  options(TETRAD_DIR = NULL)

  # Set fake env var
  old_env <- Sys.getenv("TETRAD_DIR", unset = NA)
  on.exit(Sys.setenv(TETRAD_DIR = old_env), add = TRUE)

  temp_dir <- tempdir()
  Sys.setenv(TETRAD_DIR = temp_dir)

  expect_equal(get_TETRAD_DIR(), temp_dir)

  # Should also set the R option as a side effect
  expect_equal(getOption("TETRAD_DIR"), temp_dir)
})

test_that("get_TETRAD_DIR returns NULL when env var is set but directory does not exist", {
  old_opt <- getOption("TETRAD_DIR")
  on.exit(options(TETRAD_DIR = old_opt), add = TRUE)
  options(TETRAD_DIR = NULL)

  old_env <- Sys.getenv("TETRAD_DIR", unset = NA)
  on.exit(Sys.setenv(TETRAD_DIR = old_env), add = TRUE)

  Sys.setenv(TETRAD_DIR = "/path/that/does/not/exist")

  expect_null(get_TETRAD_DIR())
})

test_that("get_TETRAD_DIR returns NULL when neither option nor env var is set", {
  old_opt <- getOption("TETRAD_DIR")
  on.exit(options(TETRAD_DIR = old_opt), add = TRUE)
  options(TETRAD_DIR = NULL)

  old_env <- Sys.getenv("TETRAD_DIR", unset = NA)
  on.exit(Sys.unsetenv("TETRAD_DIR"), add = TRUE)
  Sys.unsetenv("TETRAD_DIR")

  expect_null(get_TETRAD_DIR())
})

test_that("get_java_version returns NA when java executable not found", {
  # Stub Sys.which to return empty
  mockery::stub(get_java_version, "Sys.which", function(cmd) "")

  # Stub file.exists to always return FALSE
  mockery::stub(get_java_version, "file.exists", function(path) FALSE)

  expect_identical(get_java_version(), NA_character_)
})


test_that("get_java_version returns NA when system2 errors", {
  # Fake java path
  mockery::stub(get_java_version, "Sys.which", function(cmd) "/usr/bin/java")

  # system2 throws an error
  mockery::stub(get_java_version, "system2", function(...) stop("boom"))

  expect_identical(get_java_version(), NA_character_)
})


test_that("get_java_version returns NA when system2 returns NA in first line", {
  mockery::stub(get_java_version, "Sys.which", function(cmd) "/usr/bin/java")

  # system2 returns NA vector
  mockery::stub(get_java_version, "system2", function(...) NA_character_)

  expect_identical(get_java_version(), NA_character_)
})


test_that("get_java_version returns NA when output has no quoted version", {
  mockery::stub(get_java_version, "Sys.which", function(cmd) "/usr/bin/java")

  mockery::stub(get_java_version, "system2", function(...) {
    c(
      "java version nonsense without quotes",
      "other info"
    )
  })

  expect_identical(get_java_version(), NA_character_)
})


test_that("get_java_version extracts version correctly when present", {
  mockery::stub(get_java_version, "Sys.which", function(cmd) "/usr/bin/java")

  # Ensure the file.exists check passes
  mockery::stub(get_java_version, "file.exists", function(path) TRUE)

  # Stub system2 to return a fake java version
  mockery::stub(get_java_version, "system2", function(...) {
    c('java version "17.0.9"', "more lines")
  })

  expect_equal(get_java_version(), "17.0.9")
})

# ---- Branch 1: Java not found ----
test_that("Returns message when Java is missing", {
  mockery::stub(verify_tetrad, "get_java_version", function() {
    NA_character_
  })

  out <- verify_tetrad("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_false(out$java_ok)
  expect_null(out$java_version)
  expect_match(out$message, "Java was not found")
})


# ---- Branch 2: Java < 21 ----
test_that("Returns message when Java version is too old", {
  mockery::stub(verify_tetrad, "get_java_version", function() "17.0.9")

  out <- verify_tetrad("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_false(out$java_ok)
  expect_equal(out$java_version, "17.0.9")
  expect_match(out$message, "Java >= 21 is required")
})


# ---- Branch 3: Java OK but TETRAD_DIR is NULL ----
test_that("Returns message when tetrad directory is missing", {
  mockery::stub(verify_tetrad, "get_java_version", function() "21.0.1")
  mockery::stub(verify_tetrad, "get_TETRAD_DIR", function() NULL)

  out <- verify_tetrad("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_true(out$java_ok)
  expect_equal(out$java_version, "21.0.1")
  expect_match(out$message, "Tetrad not found.")
})


# ---- Branch 4: Java OK, tetrad dir exists, JAR missing ----
test_that("Returns message when Tetrad JAR does not exist", {
  mockery::stub(verify_tetrad, "get_java_version", function() "21.0.1")
  mockery::stub(verify_tetrad, "get_TETRAD_DIR", function() "/fake/path")

  # JAR missing
  mockery::stub(verify_tetrad, "file.exists", function(...) FALSE)

  out <- verify_tetrad("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_true(out$java_ok)
  expect_equal(out$java_version, "21.0.1")
  expect_match(out$message, "Tetrad version 1.0 not found")
})


# ---- Branch 5: Java OK, tetrad dir exists, JAR exists → SUCCESS ----
test_that("Successful detection when Java and Tetrad are OK", {
  mockery::stub(verify_tetrad, "get_java_version", function() "22.1.0")
  mockery::stub(verify_tetrad, "get_TETRAD_DIR", function() "/fake/path")

  # JAR exists
  mockery::stub(verify_tetrad, "file.exists", function(...) TRUE)

  out <- verify_tetrad("2.0")
  expect_output_list(out)

  expect_true(out$installed)
  expect_true(out$java_ok)
  expect_equal(out$version, "2.0")
  expect_equal(out$java_version, "22.1.0")
  expect_match(out$message, "Tetrad found")
  expect_match(out$message, "Java version 22.1.0")
})
