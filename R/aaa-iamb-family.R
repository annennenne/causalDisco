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
#'   \item [BnlearnSearch] for \pkg{bnlearn}.
#' }
#'
#' @example inst/roxygen-examples/iamb-family-example.R
#'
#' @references
#' I. Tsamardinos, C. F. Aliferis, and A. Statnikov. Algorithms for large scale Markov blanket
#' discovery. In Proceedings of the Sixteenth International Florida Artificial Intelligence
#' Research Society Conference, pages 376-381. AAAI Press, 2003.
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
NULL
