# ──────────────────────────────────────────────────────────────────────────────
# init_java()
# ──────────────────────────────────────────────────────────────────────────────

test_that("init_java starts JVM once and adds the specific jar", {
  with_mock_rjava({
    pkg <- "causalDisco"
    ns <- asNamespace(pkg)

    # ---- stub: return only tmp --------------------------------------
    tmp <- tempfile(fileext = ".jar")
    dir.create(dirname(tmp), recursive = TRUE, showWarnings = FALSE)
    file.create(tmp)

    # save & stub
    orig <- get("find_tetrad_jar", envir = ns)
    assignInNamespace("find_tetrad_jar", function() tmp, pkg)

    # exercise
    causalDisco:::init_java(heap = "4g")
    expect_true(.j_state$started)
    expect_true(tmp %in% .j_state$class_path)

    # exercise again to test adding same jar does not duplicate
    causalDisco:::init_java(heap = "4g")
    expect_equal(sum(.j_state$class_path == tmp), 1) # only one copy

    # final restore (cleanup)
    assignInNamespace("find_tetrad_jar", orig, pkg)
  })
})


test_that("init_java errors when no Tetrad JARs are found", {
  with_mock_rjava({
    pkg <- "causalDisco"
    ns <- asNamespace(pkg)
    orig <- get("find_tetrad_jar", envir = ns)
    withr::defer(assignInNamespace("find_tetrad_jar", orig, pkg))

    assignInNamespace("find_tetrad_jar", function() character(), pkg)

    expect_error(
      causalDisco:::init_java(heap = "2g"),
      "No Tetrad JAR found",
      fixed = TRUE
    )
  })
})
