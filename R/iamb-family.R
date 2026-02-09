#' @rdname iamb-family
#' @export
iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "iamb"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        iamb_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "iamb")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @rdname iamb-family
#' @export
iamb_fdr <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "iamb_fdr"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        iamb_fdr_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "iamb_fdr")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @rdname iamb-family
#' @export
fast_iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "fast_iamb"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        fast_iamb_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "fast_iamb")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @rdname iamb-family
#' @export
inter_iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "inter_iamb"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        inter_iamb_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "inter_iamb")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @keywords internal
iamb_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "iamb_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "iamb"
  )

  search$set_test(test, alpha)
  search$set_alg("iamb", args_to_pass)

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}

#' @keywords internal
iamb_fdr_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "iamb_fdr_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "iamb_fdr"
  )

  search$set_test(test, alpha)
  search$set_alg("iamb_fdr", args_to_pass)

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}

#' @keywords internal
fast_iamb_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "fast_iamb_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "fast_iamb"
  )

  search$set_test(test, alpha)
  search$set_alg("fast_iamb", args_to_pass)

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}

#' @keywords internal
inter_iamb_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "inter_iamb_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "inter_iamb"
  )

  search$set_test(test, alpha)
  search$set_alg("inter_iamb", args_to_pass)

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}
