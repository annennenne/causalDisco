#' Simulated data example
#'
#' A small simulated data example intended to showcase the TPC algorithm. Note that the variable name
#' prefixes defines with period they are related to ("child", "youth" or "oldage").
#'
#' @format A data.frame with 200 rows and 6 variables.
#' \describe{
#'  \item{child_x1}{Structural equation: \eqn{X_1 := \epsilon_1} with \eqn{\epsilon_1 \sim \mathrm{Unif}{0,1}}}
#'  \item{child_x2}{Structural equation: \eqn{X_2 := 2 * X_1 + \epsilon_2} with \eqn{\epsilon_2 \sim N(0,1)}}
#'  \item{youth_x3}{Structural equation: \eqn{X_3 := \epsilon3} with \eqn{\epsilon_3 \sim \mathrm{Unif}{0, 1}}}
#'  \item{youth_x4}{Structural equation: \eqn{X_4 := X_2 + \epsilon_4} with \eqn{\epsilon_4  \sim  N(0,1)}}
#'  \item{oldage_x5}{Structural equation: \eqn{X_5 := X_3^2 + X_3 - 3 * X_2 + \epsilon_5} with \eqn{\epsilon_5 \sim N(0,1)}}
#'  \item{oldage_x6}{Structural equation: \eqn{X_6 := X_4^3 + X_4^2 + 2 * X_5 + \epsilon_6} with \eqn{\epsilon_6 \sim N(0,1)}}
#'  }
#'
#' @references Petersen, AH; Osler, M and Ekstrøm, CT (2021): Data-Driven Model Building for Life-Course Epidemiology,
#'  American Journal of Epidemiology.
#' @examples
#' data(tpc_example)
#'
"tpc_example"

#' @title Causal Disco package
#' @name causalDisco-package
#' @section System requirements:
#' Requires Java (version 21 or newer) to run the Tetrad Java library.
#' Requires Cargo (Rust's package manager), rustc >= 1.80.0, xz
"_PACKAGE"
