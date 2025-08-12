library(testthat)
library(withr)

test_that("init_java starts JVM once and adds jars", {
  with_mock_rjava({
    pkg <- "causalDisco"
    ns <- asNamespace(pkg)

    # ---- first stub: return only tmp --------------------------------
    tmp <- tempfile(fileext = ".jar")
    dir.create(dirname(tmp), recursive = TRUE, showWarnings = FALSE)
    file.create(tmp)

    # save & stub
    orig <- get("find_tetrad_jars", envir = ns)
    assignInNamespace("find_tetrad_jars", function() tmp, pkg)

    # exercise
    causalDisco:::init_java(heap = "4g")
    expect_true(.j_state$started)
    expect_true(tmp %in% .j_state$class_path)

    # restore
    assignInNamespace("find_tetrad_jars", orig, pkg)

    # ---- second stub: return tmp and tmp2 ---------------------------
    tmp2 <- tempfile(fileext = ".jar")
    file.create(tmp2)
    assignInNamespace("find_tetrad_jars", function() c(tmp, tmp2), pkg)

    # exercise again
    causalDisco:::init_java(heap = "4g")
    expect_true(all(c(tmp, tmp2) %in% .j_state$class_path))

    # final restore (cleanup)
    assignInNamespace("find_tetrad_jars", orig, pkg)
  })
})

test_that("init_java errors when no Tetrad JARs are found", {
  with_mock_rjava({
    pkg <- "causalDisco"
    ns <- asNamespace(pkg)
    orig <- get("find_tetrad_jars", envir = ns)
    defer(assignInNamespace("find_tetrad_jars", orig, pkg))

    assignInNamespace("find_tetrad_jars", function() character(), pkg)

    expect_error(
      causalDisco:::init_java(heap = "2g"),
      "No Tetrad JARs found",
      fixed = TRUE
    )
  })
})
