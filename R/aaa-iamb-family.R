#' @title IAMB Family of Causal Discovery Algorithms
#' @name iamb-family
#'
#' @description
#' Functions for causal discovery using variants of the Incremental Association algorithm:
#' \itemize{
#'   \item \code{iamb}: Incremental Association (IAMB)
#'   \item \code{inter_iamb}: Interleaved Incremental Association (Inter-IAMB)
#'   \item \code{iamb_fdr}: Incremental Association with FDR (IAMB-FDR)
#'   \item \code{fast_iamb}: Fast Incremental Association (Fast-IAMB)
#' }
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"bnlearn"}}{\pkg{bnlearn} R package.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine (e.g., test or algorithm parameters).
#'
#' @details
#' Each function supports the same engines and parameters. For details on tests
#' and parameters for each engine, see:
#' \itemize{
#'   \item \code{\link{BnlearnSearch}} for \pkg{bnlearn}.
#' }
#'
#' @example inst/roxygen-examples/iamb-family-example.R
#'
#' @return
#' Each function returns a `caugi` object (of class "PDAG") and a `knowledge` object.
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
NULL
