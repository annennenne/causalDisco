# Ensure Tetrad is installed for local testing.
# - Skip Tetrad tests on CRAN (avoid installing Tetrad on CRAN).
# This also avoids rare memory issues on Tetrad (only observed on 7.6.8 and not on current version though).
# So maybe fine for the current version (7.6.10 as time of writing), but being safe for now.
# - GitHub Actions workflow uses .github/workflows/install-tetrad.R.

# Copy pasted from testthat internal function:
on_cran <- function() {
  env <- Sys.getenv("NOT_CRAN")
  if (identical(env, "")) {
    !interactive()
  } else {
    !isTRUE(as.logical(env))
  }
}

if (!on_cran()) {
  status <- verify_tetrad()
  java_ok <- status$java_ok
  tetrad_installed <- status$installed
  if (!java_ok) {
    tryCatch(
      install_java(),
      error = function(e) {
        paste("Java installation skipped:", e$message)
      }
    )
  }
  if (!tetrad_installed) {
    install_tetrad()
  }
} else {
  message("Skipping Tetrad installation on CRAN")
}

# Needed for CRAN CPU note. Note, not sufficient to set environment variable, as seen here:
# https://stackoverflow.com/questions/27319619/how-can-i-set-the-number-of-openmp-threads-from-within-the-program
RhpcBLASctl::omp_set_num_threads(2)
RhpcBLASctl::blas_set_num_threads(2)
