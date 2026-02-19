#' Prepare inputs for constraint-based algorithms
#' @description
#' Internal function to prepare and validate inputs for constraint-based algorithms.
#'
#' @inheritParams tpc_run
#' @param function_name Name of the calling function (for error messages).
#' @return A list containing the prepared inputs:
#'  \item{data}{The (possibly modified) data frame.}
#'  \item{knowledge}{The `Knowledge` object.}
#'  \item{vnames}{The variable names.}
#'  \item{suff_stat}{The sufficient statistics.}
#'  \item{na_method}{The method for handling NAs.}
#' @keywords internal
#' @noRd
constraint_based_prepare_inputs <- function(
  data = NULL,
  knowledge = NULL,
  varnames = NULL,
  na_method = "none",
  test = reg_test,
  suff_stat = NULL,
  directed_as_undirected = FALSE,
  function_name
) {
  # check required packages
  .check_if_pkgs_are_installed(
    pkgs = c("methods", "pcalg", "stats", "tidyselect"),
    function_name = function_name
  )

  # NA method validation
  if (!(na_method %in% c("none", "cc", "twd"))) {
    stop("Invalid choice of method for handling NA values.")
  }
  if (is.null(data) && is.null(suff_stat)) {
    stop("Either data or sufficient statistic must be supplied.")
  }

  if (is.null(knowledge)) {
    knowledge <- knowledge()
  }
  is_knowledge(knowledge)

  # NA handling
  if (!is.null(data) && any(is.na(data))) {
    if (na_method == "none") {
      stop(
        "Inputted data contain NA values, but no method for handling missing NAs was supplied."
      )
    } else if (na_method == "cc") {
      data <- stats::na.omit(data)
      if (nrow(data) == 0) {
        stop(
          "Complete case analysis chosen, but inputted data contain no complete cases."
        )
      }
    }
  }

  # variable names
  vnames <- if (is.null(data)) varnames else names(data)
  if (is.null(vnames) || !length(vnames)) {
    stop("Could not determine variable names. Supply `data` or `varnames`.")
  }

  # ensure all vars appear in knowledge
  missing_vars <- setdiff(vnames, knowledge$vars$var)
  if (length(missing_vars)) {
    knowledge <- add_vars(knowledge, missing_vars)
  }
  bad_vars <- setdiff(knowledge$vars$var, vnames)
  if (length(bad_vars)) {
    stop(
      "Knowledge contains variables not present in `data`: ",
      paste(bad_vars, collapse = ", ")
    )
  }

  # sufficient statistics
  if (is.null(suff_stat)) {
    if (identical(test, reg_test)) {
      suff_stat <- make_suff_stat(data, type = "reg_test")
    } else if (identical(test, cor_test)) {
      suff_stat <- make_suff_stat(data, type = "cor_test")
    } else {
      stop("suff_stat needs to be supplied when using a non-builtin test.")
    }
  } else {
    na_method <- "none"
  }

  # Wrap test function to ensure it has camelCase argument for pcalg
  internal_test <- test
  if ("suff_stat" %in% names(formals(test))) {
    # wrap snake_case -> camelCase
    internal_test <- function(x, y, S, suffStat) {
      test(x, y, conditioning_set = S, suff_stat = suffStat)
    }
  }

  list(
    data = data,
    knowledge = knowledge,
    vnames = vnames,
    suff_stat = suff_stat,
    na_method = na_method,
    internal_test = internal_test
  )
}
