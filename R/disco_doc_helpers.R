#' @title Disco Recommendation Note
#' @name disco_note
#' @section Recommendation:
#' While it is possible to call the function returned directly with a data frame,
#' we recommend using [disco()]. This provides a consistent interface and handles knowledge
#' integration.
#' @keywords internal
NULL

#' @title Causal Discovery Algorithm Return Value DAG
#' @name disco_algs_return_doc_dag
#' @section Value:
#' A function that takes a single argument `data` (a data frame). When called,
#' this function returns a list containing:
#' \itemize{
#'   \item `knowledge` A `Knowledge` object with the background knowledge
#'     used in the causal discovery algorithm. See [knowledge()] for how to construct it.
#'   \item `caugi` A [`caugi::caugi`] object (of class `DAG`) representing the learned causal graph
#'     from the causal discovery algorithm.
#' }
#' @keywords internal
NULL

#' @title Causal Discovery Algorithm Return Value PDAG
#' @name disco_algs_return_doc_pdag
#' @section Value:
#' A function that takes a single argument `data` (a data frame). When called,
#' this function returns a list containing:
#' \itemize{
#'   \item `knowledge` A `Knowledge` object with the background knowledge
#'     used in the causal discovery algorithm. See [knowledge()] for how to construct it.
#'   \item `caugi` A [`caugi::caugi`] object (of class `PDAG`) representing the learned causal graph
#'     from the causal discovery algorithm.
#' }
#' @keywords internal
NULL

# TODO: When caugi supports PAGs, we can update this to reflect that.
# For now, we use UNKNOWN to represent algorithms that return PAGs.
#' @title Causal Discovery Algorithm Return Value PAG
#' @name disco_algs_return_doc_pag
#' @section Value:
#' A function that takes a single argument `data` (a data frame). When called,
#' this function returns a list containing:
#' \itemize{
#'   \item `knowledge` A `Knowledge` object with the background knowledge
#'     used in the causal discovery algorithm. See [knowledge()] for how to construct it.
#'   \item `caugi` A [`caugi::caugi`] object representing the learned causal graph.
#'     This graph is a PAG (Partial Ancestral Graph), but since PAGs are not yet
#'     natively supported in `caugi`, it is currently stored with class `UNKNOWN`.
#' }
#' @keywords internal
NULL
