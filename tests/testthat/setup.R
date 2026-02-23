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

## Skipping Tetrad install on CRAN - am I even allowed to do it on CRAN?.
## GA workflows uses .github/workflows/install-tetrad.R
## Tetrad sometimes seems to sometimes have CPU wall time > 2.5
## (not allowed on CRAN). Only when initializing Java this seems to happen?
## E.g. I had a CRAN note appear on debian because user+system in TetradSearch examples was 2.8s,
## and it elapsed 1.1sec, and 2.8/1.1 > 2.5.

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
