#' Regression-based information loss test
#'
#' @inheritParams corTest
#'
regTest <- function(x, y, S, suffStat) {
  p1 <- regTestEachDir(x, y, S, suffStat)
  p2 <- regTestEachDir(y, x, S, suffStat)

  max(p1,p2)
}



############################################################################
## Not exported below ######################################################
############################################################################

#note: x, y, S are indexes of vars

#' @importFrom splines ns
#' @importFrom stats glm
regTestEachDir <- function(x, y, S, suffStat) {

  #args <- suffStat$otherArgs
  #if (!is.null(args) && "df" %in% names(args)) {
  #  dfs <- args$df
  #} else

  #dfs
  dfs <- 3
  dfString <- paste(", df = ", dfs, ")", sep  = "")

  #Unpack suffStat
  data <- suffStat$data
  binary <- suffStat$binary
  vnames <- names(data)
  #order <- suffStat$order

  #Choose exponential family according to ys type
  if (binary[y]) {
    fam <- "binomial"
  } else fam <- "gaussian"

  #Store info: is x binary?
  binx <- binary[x]

  #Store info: which S are binary?
  binS <- intersect(S, which(binary))
  numS <- setdiff(S, binS)

  #extract variable names
  x <- vnames[x]
  y <- vnames[y]
  S_bin <- vnames[binS]
  S_num <- vnames[numS]
  allS <- c(S_bin, S_num)

  #add spline to num x, factor to binary x
  if (!binx) {
    if (dfs > 0) x <- paste("ns(", x, dfString, sep = "")
    #    x <- paste("ns(", x, dfString, sep = "")
  } else x <- paste("factor(", x, ")", sep = "")


  #add spline to num S, factor to binary s
  #S_num <- NULL
  if (length(S_num > 0)) {
    S_num <- paste("ns(", S_num, dfString, sep = "")
    # browser()
  }
  if (length(S_bin > 0)) S_bin <- paste("factor(", S_bin, ")", sep = "")
  S <- c(S_bin, S_num, "1")

  #wrap factor around binary f
  if (fam == "binomial") {
    y <- paste("factor(", y, ")", sep = "")
  }

  #make formulas
  f1 <- as.formula(paste(y, "~", paste(S, collapse = " + ")))
  f2 <- update(f1, as.formula(paste(". ~ . + ", x, sep = "")))

  #troubleshooting
  # wp <- FALSE

  #fit models
  m1 <- suppressWarnings(glm(f1, data = data, family = fam))
  m2 <- suppressWarnings(glm(f2, data = data, family = fam))

  #troubleshooting
  # if (m1$boundary | m2$boundary) browser()

  #if convergence fails, output 0 (corresponds to no sep)
  if (!m1$converged | !m2$converged) return(0)

  #test
  anova.glm(m1, m2, test = "LRT")$`Pr(>Chi)`[2]
}
