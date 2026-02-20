# - GitHub Actions workflow uses .github/workflows/install-tetrad.R. to install Tetrad.

# Needed for CRAN CPU note. Note, not sufficient to set environment variable, as seen here:
# https://stackoverflow.com/questions/27319619/how-can-i-set-the-number-of-openmp-threads-from-within-the-program
RhpcBLASctl::omp_set_num_threads(2)
RhpcBLASctl::blas_set_num_threads(2)

status <- verify_tetrad()
java_ok <- status$java_ok
tetrad_installed <- status$installed
if (!java_ok) {
  message(
    "Java not found or version is too old. Skipping tests requiring rJava"
  )
}
if (!tetrad_installed) {
  message("Tetrad not installed. Skipping tests requiring Tetrad.")
}
