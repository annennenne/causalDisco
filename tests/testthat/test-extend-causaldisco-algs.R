test_that("new_disco_method creates a valid disco method", {
  builder <- function(knowledge = NULL) {
    list(
      set_knowledge = function(k) NULL,
      run = function(data) NULL
    )
  }

  method <- new_disco_method(
    builder = builder,
    name = "test_alg",
    engine = "bnlearn",
    graph_class = "PDAG"
  )

  # S3 class
  expect_true(inherits(method, "disco_method"))
  expect_identical(class(method)[1], "test_alg")

  # attributes
  expect_identical(attr(method, "engine"), "bnlearn")
  expect_identical(attr(method, "graph_class"), "PDAG")

  # callable
  expect_type(method, "closure")
})

test_that("new_disco_method uses the builder to run the algorithm", {
  ran <- FALSE

  builder <- function(knowledge = NULL) {
    list(
      set_knowledge = function(k) NULL,
      run = function(data) {
        ran <<- TRUE
        data
      }
    )
  }

  method <- new_disco_method(
    builder = builder,
    name = "test_alg",
    engine = "bnlearn",
    graph_class = "PDAG"
  )

  df <- data.frame(x = 1)
  out <- method(df)

  expect_true(ran)
  expect_identical(out, df)
})


test_that("distribute_engine_args delegates to check_args_and_distribute_args", {
  fake_return <- list(a = 1, b = 2)

  search <- list()
  args <- list(x = 1)
  engine <- "bnlearn"
  alg <- "hpc"

  with_mocked_bindings(
    check_args_and_distribute_args = function(s, a, e, al) {
      expect_identical(s, search)
      expect_identical(a, args)
      expect_identical(e, engine)
      expect_identical(al, alg)
      fake_return
    },
    {
      out <- distribute_engine_args(search, args, engine, alg)
      expect_identical(out, fake_return)
    }
  )
})

test_that("distribute_engine_args propagates errors", {
  with_mocked_bindings(
    check_args_and_distribute_args = function(...) {
      stop("boom", call. = FALSE)
    },
    {
      expect_error(
        distribute_engine_args(list(), list(), "bnlearn", "hpc"),
        "boom"
      )
    }
  )
})

test_that("register_tetrad_algorithm registers a new algorithm", {
  reset_tetrad_alg_registry()

  setup_fun <- function(search, param1 = 1) {
    search$set_alg("custom_alg")
    search$set_param1(param1)
  }

  register_tetrad_algorithm("custom_alg", setup_fun)

  registered_fun <- tetrad_alg_registry[["custom_alg"]]
  expect_identical(registered_fun, setup_fun)
  reset_tetrad_alg_registry()
})

test_that("register_tetrad_algorithm errors if not a function", {
  reset_tetrad_alg_registry()

  expect_error(
    register_tetrad_algorithm("not_a_function", "I am not a function"),
    "must be a function"
  )
})
