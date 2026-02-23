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
  message(
    "Tetrad not installed. Skipping tests requiring Tetrad."
  )
}

## Copy pasted from testthat internal function:
# on_cran <- function() {
#   env <- Sys.getenv("NOT_CRAN")
#   if (identical(env, "")) {
#     !interactive()
#   } else {
#     !isTRUE(as.logical(env))
#   }
# }

## We only install Tetrad if not on CRAN. It can in very very rare cases run out of memory, and thus test fail, and
## Tetrad uses more than 2 cores, which isn't fixed yet (not allowed on CRAN).
# if (java_ok && !tetrad_installed) {
#   if (!on_cran()) {
#     install_tetrad()
#     status <- verify_tetrad()
#     tetrad_installed <- status$installed
#   } else {
#     message(
#       "Running on CRAN. Skipping Tetrad installation and tests requiring Tetrad."
#     )
#   }
# }
