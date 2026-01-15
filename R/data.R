#' Simulated data example
#'
#' A small simulated data example intended to showcase the TPC algorithm. Note that the variable name
#' prefixes defines which period they are related to ("child", "youth" or "oldage").
#'
#' @format A data.frame with 200 rows and 6 variables.
#' \describe{
#'  \item{child_x1}{Structural equation: \eqn{X_1 := \epsilon_1} with \eqn{\epsilon_1 \sim \mathrm{Unif}\{0,1\}}}
#'  \item{child_x2}{Structural equation: \eqn{X_2 := 2 \cdot X_1 + \epsilon_2} with \eqn{\epsilon_2 \sim N(0,1)}}
#'  \item{youth_x3}{Structural equation: \eqn{X_3 := \epsilon_3} with \eqn{\epsilon_3 \sim \mathrm{Unif}\{0, 1\}}}
#'  \item{youth_x4}{Structural equation: \eqn{X_4 := X_2 + \epsilon_4} with \eqn{\epsilon_4  \sim  N(0,1)}}
#'  \item{oldage_x5}{Structural equation: \eqn{X_5 := X_3^2 + X_3 - 3 \cdot X_2 + \epsilon_5} with \eqn{\epsilon_5 \sim N(0,1)}}
#'  \item{oldage_x6}{Structural equation: \eqn{X_6 := X_4^3 + X_4^2 + 2 \cdot X_5 + \epsilon_6} with \eqn{\epsilon_6 \sim N(0,1)}}
#'  }
#'
#' @references Petersen, AH; Osler, M and Ekstrøm, CT (2021): Data-Driven Model Building for Life-Course Epidemiology,
#'  American Journal of Epidemiology.
#' @examples
#' data(tpc_example)
#'
"tpc_example"

#' Simulated numerical data example
#'
#' @format A data.frame with 1000 rows and 5 variables.
#' \describe{
#'  \item{X1}{Structural equation: \eqn{X_1 := \sqrt{Z} + \epsilon_1} with \eqn{\epsilon_1 \sim \mathrm{Unif}[0, 2]}}
#'  \item{X2}{Structural equation: \eqn{X_2 := 2 \cdot X_3 - \epsilon_2} with \eqn{\epsilon_2 \sim N(5, 1)}}
#'  \item{X3}{Structural equation: \eqn{X_3 := \epsilon_3} with \eqn{\epsilon_3 \sim \mathrm{Unif}[5, 10]}}
#'  \item{Z}{Structural equation: \eqn{Z := |\epsilon_4|} with \eqn{\epsilon_4 \sim N(10, 1)}}
#'  \item{Y}{Structural equation: \eqn{Y := X_1^2 + X_2 - X_3 - Z + \epsilon_5} with \eqn{\epsilon_5 \sim N(10, 1)}}
#' }
#'
#' @details
#' The R code used to generate this dataset is as follows:
#'
#' ```R
#' set.seed(1405)
#' n <- 1000
#' Z <- abs(rnorm(n, mean = 10))
#' X1 <- sqrt(Z) + runif(n, min = 0, max = 2)
#' X3 <- runif(n, min = 5, max = 10)
#' X2 <- 2 * X3 - rnorm(n, mean = 5)
#' Y  <- X1^2 + X2 - X3 - Z + rnorm(n, mean = 10)
#' num_data <- data.frame(X1, X2, X3, Z, Y)
#' ```
#'
#' @examples
#' data(num_data)
#'
"num_data"

#' Simulated categorical data example
#'
#' A dataset created by discretizing the continuous `num_data` into 5 categorical levels per variable.
#'
#' @format A data.frame with 1000 rows and 5 variables.
#' \describe{
#'  \item{X1}{Categorical version of `num_data$X1`, with 5 levels a–e.}
#'  \item{X2}{Categorical version of `num_data$X2`, with 5 levels a–e.}
#'  \item{X3}{Categorical version of `num_data$X3`, with 5 levels a–e.}
#'  \item{Z}{Categorical version of `num_data$Z`, with 5 levels a–e.}
#'  \item{Y}{Categorical version of `num_data$Y`, with 5 levels a–e.}
#' }
#'
#' @details
#' The R code used to generate this dataset is as follows:
#'
#' ```R
#' data(num_data)
#' cat_data <- as.data.frame(sapply(num_data, function(x) cut(x, breaks = 5, labels = letters[1:5])))
#' ```
#'
#' @examples
#' data(cat_data)
#'
#' @seealso [num_data]
"cat_data"

#' Simulated mixed data example
#'
#' A dataset combining continuous and categorical variables. The first three variables are replaced
#' with categorical versions from `cat_data`.
#'
#' @format A data.frame with 1000 rows and 5 variables.
#' \describe{
#'  \item{X1}{Categorical, from `cat_data$X1`.}
#'  \item{X2}{Categorical, from `cat_data$X2`.}
#'  \item{X3}{Categorical, from `cat_data$X3`.}
#'  \item{Z}{Numeric, same as `num_data$Z`.}
#'  \item{Y}{Numeric, same as `num_data$Y`.}
#' }
#'
#' @details
#' The R code used to generate this dataset is as follows:
#'
#' ```R
#' data(num_data)
#' data(cat_data)
#' mix_data <- num_data
#' mix_data$X1 <- cat_data$X1
#' mix_data$X2 <- cat_data$X2
#' mix_data$X3 <- cat_data$X3
#' ```
#'
#' @examples
#' data(mix_data)
#'
#' @seealso [num_data], [cat_data]
"mix_data"

#' Simulated categorical data with MCAR (missing completely at random)
#'
#' A dataset based on `cat_data` where some values are randomly removed to simulate MCAR.
#'
#' @format A data.frame with 1000 rows and 5 variables.
#' \describe{
#'  \item{X1}{Categorical, 100 values set to NA (MCAR).}
#'  \item{X2}{Categorical, 50 values set to NA (MCAR).}
#'  \item{X3}{Categorical, 200 values set to NA (MCAR).}
#'  \item{Z}{Categorical, no missing values.}
#'  \item{Y}{Categorical, no missing values.}
#' }
#'
#' @details
#' The R code used to generate this dataset is as follows:
#'
#' ```R
#' data(cat_data)
#' cat_data_mcar <- cat_data
#' set.seed(1405)
#' cat_data_mcar$X1[sample(1:n, 100)] <- NA
#' cat_data_mcar$X2[sample(1:n, 50)] <- NA
#' cat_data_mcar$X3[sample(1:n, 200)] <- NA
#' ```
#'
#' @examples
#' data(cat_data_mcar)
#'
#' @seealso [cat_data]
"cat_data_mcar"

#' Simulated numerical data example with latent variable
#'
#' A dataset similar to `num_data` but with the variable `Z` treated as a latent variable and thus omitted.
#'
#' @format A data.frame with 1000 rows and 4 variables.
#' \describe{
#'  \item{X1}{Structural equation: \eqn{X_1 := \sqrt{Z} + \epsilon_1} with \eqn{\epsilon_1 \sim \mathrm{Unif}[0, 2]}}
#'  \item{X2}{Structural equation: \eqn{X_2 := 2 \cdot X_3 - \epsilon_2} with \eqn{\epsilon_2 \sim N(5, 1)}}
#'  \item{X3}{Structural equation: \eqn{X_3 := \epsilon_3} with \eqn{\epsilon_3 \sim \mathrm{Unif}[5, 10]}}
#'  \item{Z}{Structural equation: \eqn{Z := |\epsilon_4|} with \eqn{\epsilon_4 \sim N(10, 1)}}
#'  \item{Y}{Structural equation: \eqn{Y := X_1^2 + X_2 - X_3 - Z + \epsilon_5} with \eqn{\epsilon_5 \sim N(10, 1)}}
#' }
#'
#' @details
#' The R code used to generate this dataset is as follows:
#'
#' ```R
#' data(num_data)
#' num_data_latent <- num_data[, c("X1", "X2", "X3", "Y")]
#' ```
#'
#' @examples
#' data(num_data_latent)
#'
#' @seealso [num_data]
"num_data_latent"
