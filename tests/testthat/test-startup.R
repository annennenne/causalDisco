pkg <- "causalDisco"

test_that(".onLoad sets default heap when option is absent", {
  local_options(java.heap.size = NULL)

  with_mocked_bindings(
    default_heap = function() "2g",
    .package = pkg,
    {
      .onLoad(libname = "", pkgname = pkg)
      expect_equal(getOption("java.heap.size"), "2g")
    }
  )
})

test_that(".onAttach prompts and sets heap when interactive and option/env missing", {
  local_options(java.heap.size = NULL)
  local_envvar(JAVA_HEAP_SIZE = "")
  called <- FALSE
  seen_heap <- NULL

  with_mocked_bindings(
    is_interactive = function() TRUE,
    ask_heap_size = function() "4g",
    parse_heap_gb = function(x) as.numeric(sub("g$", "", x)),
    init_java = function(heap) {
      called <<- TRUE
      seen_heap <<- heap
      invisible(NULL)
    },
    current_heap_gb = function() 4,
    .package = pkg,
    {
      expect_message(
        .onAttach(libname = "", pkgname = pkg),
        "causalDisco is inintalized with Java heap size 4gb",
        class = "packageStartupMessage"
      )
      expect_true(called)
      expect_equal(seen_heap, "4g")
      expect_equal(getOption("java.heap.size"), "4g")
    }
  )
})

test_that(".onAttach canonicalises heap option to 'Ng'", {
  local_envvar(JAVA_HEAP_SIZE = "")
  local_options(java.heap.size = "4096m")

  with_mocked_bindings(
    is_interactive = function() FALSE,
    parse_heap_gb = function(x) 4,
    init_java = function(heap) invisible(NULL),
    current_heap_gb = function() 4,
    .package = pkg,
    {
      expect_message(
        .onAttach(libname = "", pkgname = pkg),
        "Java heap size 4gb",
        class = "packageStartupMessage"
      )
      expect_equal(getOption("java.heap.size"), "4g")
    }
  )
})

test_that(".onAttach warns when runtime heap != requested", {
  local_envvar(JAVA_HEAP_SIZE = "")
  local_options(java.heap.size = "2g")

  with_mocked_bindings(
    is_interactive = function() FALSE,
    parse_heap_gb = function(x) 2,
    init_java = function(heap) invisible(NULL),
    current_heap_gb = function() 6,
    .package = pkg,
    {
      expect_warning(
        .onAttach(libname = "", pkgname = pkg),
        "Java heap is 6 GB but you requested 2 GB",
        fixed = TRUE
      )
    }
  )
})

test_that(".onAttach does not prompt in non-interactive mode", {
  local_envvar(JAVA_HEAP_SIZE = "")
  local_options(java.heap.size = "3g")
  prompted <- FALSE

  with_mocked_bindings(
    is_interactive = function() FALSE,
    ask_heap_size = function() {
      prompted <<- TRUE
      "8g"
    },
    parse_heap_gb = function(x) as.numeric(sub("g$", "", x)),
    init_java = function(heap) invisible(NULL),
    current_heap_gb = function() 3,
    .package = pkg,
    {
      .onAttach(libname = "", pkgname = pkg)
      expect_false(prompted)
      expect_equal(getOption("java.heap.size"), "3g")
    }
  )
})

test_that(".onAttach does not prompt when env var set", {
  local_options(java.heap.size = NULL)
  local_envvar(JAVA_HEAP_SIZE = "5g")
  prompted <- FALSE

  with_mocked_bindings(
    is_interactive = function() TRUE,
    ask_heap_size = function() {
      prompted <<- TRUE
      "9g"
    },
    parse_heap_gb = function(x) 5,
    init_java = function(heap) invisible(NULL),
    current_heap_gb = function() 5,
    .package = pkg,
    {
      .onAttach(libname = "", pkgname = pkg)
      expect_false(prompted)
      expect_equal(getOption("java.heap.size"), "5g")
    }
  )
})

test_that("ask_heap_size returns default on empty input", {
  with_mocked_bindings(
    .read_line = function(prompt) "",
    .package = pkg,
    {
      expect_equal(ask_heap_size(), "2g")
    }
  )
})

test_that("ask_heap_size returns Ng for numeric input", {
  with_mocked_bindings(
    .read_line = function(prompt) "4",
    .package = pkg,
    {
      expect_equal(ask_heap_size(), "4g")
    }
  )
})

test_that("ask_heap_size reprompts on invalid input and messages once", {
  answers <- c("abc", "8")
  i <- 0
  with_mocked_bindings(
    .read_line = function(prompt) {
      i <<- i + 1
      answers[[i]]
    },
    .package = pkg,
    {
      expect_message(
        {
          out <- ask_heap_size()
          expect_equal(out, "8g")
        },
        "Please enter a positive integer such as 4, 8, 16, etc.",
        fixed = TRUE
      )
    }
  )
})

test_that(".check_if_pkgs_are_installed returns TRUE (invisibly) when packages exist", {
  expect_true(isTRUE(.check_if_pkgs_are_installed(
    pkgs = c("utils", "stats"),
    function_name = "some_function"
  )))
  expect_invisible(.check_if_pkgs_are_installed(
    pkgs = c("utils", "stats"),
    function_name = "some_function"
  ))
})

test_that(".check_if_pkgs_are_installed errors if neither function_name nor class_name is provided", {
  expect_error(
    .check_if_pkgs_are_installed(pkgs = "utils"),
    "Either function_name or class_name must be provided",
    fixed = FALSE
  )
})

test_that(".check_if_pkgs_are_installed errors with helpful message for missing pkgs (function case)", {
  fake <- "definitelyNotARealPkg_abcdef"
  expect_error(
    .check_if_pkgs_are_installed(
      pkgs = c("utils", fake),
      function_name = "needs_pkgs"
    ),
    "The following packages are required for `needs_pkgs()` but are not installed:",
    fixed = T
  )
})

test_that(".check_if_pkgs_are_installed errors with helpful message for missing pkgs (class case)", {
  fake <- "definitelyNotARealPkg_ghijkl"
  expect_error(
    .check_if_pkgs_are_installed(
      pkgs = c("stats", fake),
      class_name = "CoolR6Class"
    ),
    paste0("The following packages are required for the R6 class `CoolR6Class` but are not installed"),
    fixed = FALSE
  )
})
