# ──────────────────────────────────────────────────────────────────────────────
# init_java()
# ──────────────────────────────────────────────────────────────────────────────

test_that("init_java errors when no Tetrad JARs are found", {
  java_ok <- verify_tetrad()$java_ok
  if (!java_ok) {
    skip("Java not available; skipping init_java tests")
  }

  with_mock_rjava({
    pkg <- "causalDisco"
    ns <- asNamespace(pkg)
    orig <- get("get_tetrad_dir", envir = ns)
    withr::defer(assignInNamespace("get_tetrad_dir", orig, pkg))

    assignInNamespace("get_tetrad_dir", function() character(), pkg)

    expect_error(
      causalDisco:::init_java(heap = "2g"),
      "No Tetrad JAR found",
      fixed = TRUE
    )
  })
})
