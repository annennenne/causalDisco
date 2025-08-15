test_that("disco_method builds a closure with correct classes and private env", {
  # fake builder that records the knowledge it was called with
  make_builder <- function() {
    function(k) {
      e <- new.env(parent = emptyenv())
      e$k <- k
      list(
        set_knowledge = function(knowledge) {
          e$k <- knowledge
          invisible(NULL)
        },
        run = function(data) {
          list(data = data, knowledge = e$k)
        }
      )
    }
  }

  builder <- make_builder()
  m <- disco_method(builder, method_class = "pc")

  # classes
  expect_s3_class(m, c("pc", "disco_method", "function"))
  # private env has builder and NULL knowledge
  env <- environment(m)
  expect_true(is.function(env$builder))
  expect_null(env$knowledge)

  # data guard
  expect_error(m(1:3), "`data` must be a data frame.", fixed = TRUE)

  # when called, passes env$knowledge (NULL) to builder and returns runner$run()
  df <- data.frame(x = 1:3, y = 3:1)
  out <- m(df)
  expect_type(out, "list")
  expect_identical(out$knowledge, NULL)
  expect_identical(out$data, df)
})

test_that("set_knowledge.disco_method returns a new method that injects knowledge", {
  # fake builder with capturable knowledge flow
  make_builder <- function() {
    function(k) {
      e <- new.env(parent = emptyenv())
      e$k <- k
      list(
        set_knowledge = function(knowledge) {
          e$k <- knowledge
          invisible(NULL)
        },
        run = function(data) {
          list(data = data, knowledge = e$k)
        }
      )
    }
  }

  builder <- make_builder()
  m <- disco_method(builder, "pc")

  # original method remains knowledge-free
  df <- data.frame(a = 1:2, b = 2:1)
  out0 <- m(df)
  expect_null(out0$knowledge)

  # set knowledge -> returns a new disco_method preserving class
  kn <- list(tag = "my-knowledge")
  m2 <- set_knowledge(m, kn)
  expect_s3_class(m2, c("pc", "disco_method", "function"))

  # the new method injects knowledge via runner$set_knowledge()
  out1 <- m2(df)
  expect_identical(out1$knowledge, kn)

  # the original method is unchanged (immutability check)
  out2 <- m(df)
  expect_null(out2$knowledge)
})

test_that("disco enforces method type and injects knowledge when provided", {
  # wrong type
  df <- data.frame(x = 1:3, y = 3:1)
  expect_error(
    disco(df, method = function(x) x),
    "The method must be a disco method object.",
    fixed = TRUE
  )

  # happy path without knowledge
  builder <- function(k) {
    e <- new.env(parent = emptyenv())
    e$k <- k
    list(
      set_knowledge = function(knowledge) {
        e$k <- knowledge
        invisible(NULL)
      },
      run = function(data) {
        list(data = data, knowledge = e$k)
      }
    )
  }
  m <- disco_method(builder, "pc")
  out <- disco(df, m)
  expect_identical(out$data, df)
  expect_null(out$knowledge)

  # with knowledge -> set_knowledge() is applied before run
  kn <- list(ok = "yes")
  out2 <- disco(df, m, knowledge = kn)
  expect_identical(out2$knowledge, kn)
})

test_that("set_knowledge wrapped method still validates data.frame input", {
  builder <- function(k) {
    e <- new.env(parent = emptyenv())
    e$k <- k
    list(
      set_knowledge = function(knowledge) {
        e$k <- knowledge
        invisible(NULL)
      },
      run = function(data) {
        list(data = data, knowledge = e$k)
      }
    )
  }
  m <- disco_method(builder, "pc")
  m2 <- set_knowledge(m, list(foo = "bar"))

  expect_error(m2(1:5), "`data` must be a data frame.", fixed = TRUE)
})
