test_that("get_tetrad_dir returns option tetrad.dir when set", {
  old_opt <- getOption("tetrad.dir")
  on.exit(options(tetrad.dir = old_opt), add = TRUE)

  options(tetrad.dir = "/opt/tetrad")

  expect_equal(get_tetrad_dir(), "/opt/tetrad")
})

test_that("get_tetrad_dir returns TETRAD_DIR env var when option is NULL and dir exists", {
  # Ensure option is NULL
  old_opt <- getOption("tetrad.dir")
  on.exit(options(tetrad.dir = old_opt), add = TRUE)
  options(tetrad.dir = NULL)

  # Set fake env var
  old_env <- Sys.getenv("TETRAD_DIR", unset = NA)
  on.exit(Sys.setenv(TETRAD_DIR = old_env), add = TRUE)

  temp_dir <- tempdir()
  Sys.setenv(TETRAD_DIR = temp_dir)

  expect_equal(get_tetrad_dir(), temp_dir)

  # Should also set the R option as a side effect
  expect_equal(getOption("tetrad.dir"), temp_dir)
})

test_that("get_tetrad_dir returns NULL when env var is set but directory does not exist", {
  old_opt <- getOption("tetrad.dir")
  on.exit(options(tetrad.dir = old_opt), add = TRUE)
  options(tetrad.dir = NULL)

  old_env <- Sys.getenv("TETRAD_DIR", unset = NA)
  on.exit(Sys.setenv(TETRAD_DIR = old_env), add = TRUE)

  Sys.setenv(TETRAD_DIR = "/path/that/does/not/exist")

  expect_null(get_tetrad_dir())
})

test_that("get_tetrad_dir returns NULL when neither option nor env var is set", {
  old_opt <- getOption("tetrad.dir")
  on.exit(options(tetrad.dir = old_opt), add = TRUE)
  options(tetrad.dir = NULL)

  old_env <- Sys.getenv("TETRAD_DIR", unset = NA)
  on.exit(Sys.unsetenv("TETRAD_DIR"), add = TRUE)
  Sys.unsetenv("TETRAD_DIR")

  expect_null(get_tetrad_dir())
})

test_that("get_java_version returns NA when java executable not found", {
  # Sys.which("java") returns empty string
  mockery::stub(get_java_version, "Sys.which", function(cmd) "")

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

  mockery::stub(get_java_version, "system2", function(...) {
    c('java version "17.0.9"', "more lines")
  })

  expect_equal(get_java_version(), "17.0.9")
})


# Helper to reduce repetition
expect_output_list <- function(x) {
  expect_type(x, "list")
  expect_named(x, c("installed", "version", "java_ok", "java_version", "message"))
}

# ---- Branch 1: Java not found ----
test_that("Returns message when Java is missing", {
  mockery::stub(check_tetrad_install, "get_java_version", function() NA_character_)

  out <- check_tetrad_install("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_false(out$java_ok)
  expect_null(out$java_version)
  expect_match(out$message, "Java was not found")
})


# ---- Branch 2: Java < 21 ----
test_that("Returns message when Java version is too old", {
  mockery::stub(check_tetrad_install, "get_java_version", function() "17.0.9")

  out <- check_tetrad_install("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_false(out$java_ok)
  expect_equal(out$java_version, "17.0.9")
  expect_match(out$message, "Java >= 21 is required")
})


# ---- Branch 3: Java OK but tetrad_dir is NULL ----
test_that("Returns message when tetrad directory is missing", {
  mockery::stub(check_tetrad_install, "get_java_version", function() "21.0.1")
  mockery::stub(check_tetrad_install, "get_tetrad_dir", function() NULL)

  out <- check_tetrad_install("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_true(out$java_ok)
  expect_equal(out$java_version, "21.0.1")
  expect_match(out$message, "Tetrad directory not configured")
})


# ---- Branch 4: Java OK, tetrad dir exists, JAR missing ----
test_that("Returns message when Tetrad JAR does not exist", {
  mockery::stub(check_tetrad_install, "get_java_version", function() "21.0.1")
  mockery::stub(check_tetrad_install, "get_tetrad_dir", function() "/fake/path")

  # JAR missing
  mockery::stub(check_tetrad_install, "file.exists", function(...) FALSE)

  out <- check_tetrad_install("1.0")
  expect_output_list(out)

  expect_false(out$installed)
  expect_true(out$java_ok)
  expect_equal(out$java_version, "21.0.1")
  expect_match(out$message, "Tetrad version 1.0 not found")
})


# ---- Branch 5: Java OK, tetrad dir exists, JAR exists → SUCCESS ----
test_that("Successful detection when Java and Tetrad are OK", {
  mockery::stub(check_tetrad_install, "get_java_version", function() "22.1.0")
  mockery::stub(check_tetrad_install, "get_tetrad_dir", function() "/fake/path")

  # JAR exists
  mockery::stub(check_tetrad_install, "file.exists", function(...) TRUE)

  out <- check_tetrad_install("2.0")
  expect_output_list(out)

  expect_true(out$installed)
  expect_true(out$java_ok)
  expect_equal(out$version, "2.0")
  expect_equal(out$java_version, "22.1.0")
  expect_match(out$message, "Tetrad found")
  expect_match(out$message, "Java version 22.1.0")
})
