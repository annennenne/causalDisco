
library(pcalg)
source("./R/discofunctions.R")

edgesFromAdjMat <- function(amat) {
  vnames <- rownames(amat)
  nvar <- dim(amat)[1]
  mat <- matrix(amat, nrow = nvar, ncol = nvar)
  outF <- data.frame(from = NULL, to = NULL)
  # browser()
  for (i in 1:nvar) {
    thisC <- mat[, i]
    if (sum(thisC) > 0) {
      outF <- rbind(outF, data.frame(from = vnames[i], to = vnames[as.logical(thisC)]))
    }
  }
  outF$from <- as.character(outF$from)
  outF$to <- as.character(outF$to)
  outF
}

amat <- function(pcres) {
  as(pcres, "amat")
}



makeBgKnowledge <- function(amat, data, order, sep = "_") {
  # browser()
  crossTimeWL <- orderedBL(data, order = rev(order), sep = sep)
  edges <- edgesFromAdjMat(amat)
  fromPrefixes <- sapply(strsplit(edges$from, split = sep), function(x) x[1])
  toPrefixes <- sapply(strsplit(edges$to, split = sep), function(x) x[1])
  crossTimeEdges <- edges[fromPrefixes != toPrefixes,]
  #edgesOut <- rbind(edges[fromPrefixes == toPrefixes,],
  #                  dplyr::intersect(crossTimeWL, crossTimeEdges))
  #edgesOut
  dplyr::intersect(crossTimeWL, crossTimeEdges)
}


####


makeSuff2 <- function(data, type = "gamTest", order = NULL) {
  if (type == "gamTest") {
    bin <- unlist(sapply(data, function(x) length(unique(x)) == 2))
    suff <- list(data = data, binary = bin)
    if (!is.null(order)) suff$order <- order
  } else suff <- data

  suff
}

#is x (strictly) after y in order?
is.after <- function(x, y, order, sep = "_") {
  prefix_x <- strsplit(x, sep)[[1]][1]
  prefix_y <- strsplit(y, sep)[[1]][1]
  res <- which(order == prefix_x) > which(order == prefix_y)
  if (length(res) == 0) res <- FALSE
  res
}

#is.after("child_x", "child_t", c("child", "adult"))

#note: x, y, S are indexes of vars
library(splines)
gamTestEachDir2 <- function(x, y, S, suffStat) {

  # browser()

  #Settings
  dfs <- 3
  dfString <- paste(", df = ", dfs, ")", sep  = "")

  #Unpack suffStat
  data <- suffStat$data
  binary <- suffStat$binary
  vnames <- names(data)
  order <- suffStat$order

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

  #check if any variables occuring after x both y are in S
  #if a problem is found, return p value of 0
  #double check: 0 or 1?
  Sproblem <- FALSE
  i <- 1
  nS <- length(allS)
  while(!Sproblem & i <= nS) {
    s <- allS[i]
    afterx <- is.after(s, x, order)
    aftery <- is.after(s, y, order)
    if (afterx & aftery) Sproblem <- TRUE
    i <- i + 1
  }
  if (Sproblem) {
    return(0)
  }

  #add spline to num x, factor to binary x
  if (!binx) {
    if (df > 0) x <- paste("ns(", x, dfString, sep = "")
  } else x <- paste("factor(", x, ")", sep = "")


  #add spline to num S, factor to binary s
  S_num <- NULL
  if (length(S_num > 0)) S_num <- paste("ns(", S_num, dfString, sep = "")
  if (length(S_bin > 0)) S_bin <- paste("factor(", S_bin, ")", sep = "")
  S <- c(S_bin, S_num, "1")

  #make formulas
  f1 <- as.formula(paste(y, "~", paste(S, collapse = " + ")))
  f2 <- update(f1, as.formula(paste(". ~ . + ", x, sep = "")))

  #troubleshooting
  # wp <- FALSE

  #fit models
  m1 <- glm(f1, data = data, family = fam)
  m2 <- glm(f2, data = data, family = fam)

  #troubleshooting
  # if (m1$boundary | m2$boundary) browser()

  #if convergence fails, output 0 (corresponds to no sep)
  if (!m1$converged | !m2$converged) return(0)

  #test
  stats:::anova.glm(m1, m2, test = "LRT")$`Pr(>Chi)`[2]
}

gamTest2 <- function(x, y, S, suffStat) {
  p1 <- gamTestEachDir2(x, y, S, suffStat)
  p2 <- gamTestEachDir2(y, x, S, suffStat)

  max(p1,p2)
}



tpdagOld <- function(skel, order, vnames) {
  tempSkelAmat <- orderRestrictAmat(amat(skel), order = order)
  skel@graph <- as(graph::ftM2adjM(as.matrix(edgesFromAdjMat(tempSkelAmat)),
                                   V = vnames),
                   "graphNEL")
  udag2pdag(skel)
}
