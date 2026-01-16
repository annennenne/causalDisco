#' Simulated data example
#' 
#' A small simulated data example intended to showcase the TPC algorithm. Note that the variable name
#' prefixes defines with period they are related to ("child", "youth" or "oldage"). 
#' 
#' @format A data.frame with 200 rows and 6 variables. 
#' \describe{
#'  \item{child_x1}{Structural equation: \eqn{X1 := \epsilon1} with \eqn{\epsilon1  ~ Unif{0,1}}}
#'  \item{child_x2}{Structural equation: \eqn{X2 := 2 * X1 + \epsilon2} with \eqn{\epsilon2  ~  N(0,1)}}
#'  \item{youth_x3}{Structural equation: \eqn{X3 := \epsilon3} with \eqn{\epsilon3  ~  Unif{0, 1}}}
#'  \item{youth_x4}{Structural equation: \eqn{X4 := X2 + \epsilon4} with \eqn{\epsilon4  ~  N(0,1)}}
#'  \item{oldage_x5}{Structural equation: \eqn{X5 := X3^2 + X3 - 3 * X2 + \epsilon5} with \eqn{\epsilon5  ~  N(0,1)}}
#'  \item{oldage_x6}{Structural equation: \eqn{X6 := X4^3 + X4^2 + 2 * X5 + \epsilon6} with \eqn{\epsilon6  ~  N(0,1)}}
#'  }
#'  
#' @references Petersen, AH; Osler, M and Ekstrøm, CT (2021): Data-Driven Model Building for Life-Course Epidemiology, 
#'  American Journal of Epidemiology. 
#' @examples 
#'  data(tpcExample)
#'  
"tpcExample"