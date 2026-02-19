# skip the whole test if Java/rJava/Tetrad jars aren't ready (extra precaution also doing skip_on_cran())
skip_if_no_tetrad <- function() {
  testthat::skip_on_cran()
  if (is.null(verify_tetrad()$version)) {
    testthat::skip("Tetrad not installed or version unknown")
  }

  if (!verify_tetrad()$java_ok) {
    testthat::skip("Java version not sufficient for Tetrad (need >= 21)")
  }

  ok <- tryCatch(
    {
      init_java()
      TRUE
    },
    error = function(e) FALSE
  )

  if (!ok) {
    testthat::skip(
      "Java/Tetrad not initialized (init_java() failed or no JARs found)"
    )
  }
}

# make Knowledge object that respects the data-generating mechanism
make_knowledge_test_object <- function(df) {
  tiered_kn <- knowledge(
    df,
    tier(
      1 ~ Z + X3,
      2 ~ X1 + X2,
      3 ~ Y
    )
  )

  forbidden_kn <- knowledge(
    df,
    Z %!-->% X3
  )

  required_kn <- knowledge(
    df,
    X1 %-->% Y
  )
  combi_kn <- tiered_kn + forbidden_kn + required_kn

  list(
    tiered_kn = tiered_kn,
    forbidden_kn = forbidden_kn,
    required_kn = required_kn,
    combi_kn = combi_kn
  )
}

# small assertion helper for Java objects
expect_jobj <- function(x) {
  testthat::expect_s4_class(x, "jobjRef")
}
