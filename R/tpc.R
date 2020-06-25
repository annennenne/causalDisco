#' Perform causal discovery using the temporal PC algorithm (TPC)
#'
#' More description here.
#'
#' @param data A data.frame
#' @param order A character vector...
#' @param sparsity A numeric vector...
#' @param test A procedure for testing conditional independence...
#' @param ... Further optional arguments which will be passed to the
#' testing procedure.
#'
#' @return Value blablabla tpdag
#'
#'
#' @export
tpc <- function(data, order, sparsity, test = NULL, ...) {
  vnames <- names(data)
  #browser()

  skel <- pcalg::skeleton(suffStat = makeSuffStat(data, order = order, ...),
                   indepTest = gamTest, ###!!!
                   alpha = sparsity[1],
                   labels = vnames, ###!!!
                   method = "stable.fast")
  res <- tpdag(skel, order = order, vnames = vnames)

  out <- list(amat = amat(res), order = order, psi = sparsity,
              ntests = NULL #fix
              )
  class(out) <- "tpdag"
  out
}










############################################################################
## Not exported below ######################################################
############################################################################




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


makeSuffStat <- function(data, type = "gamTest", order = NULL, ...) {
  #browser()
  if (type == "gamTest") {
    bin <- unlist(sapply(data, function(x) length(unique(x)) == 2))
    suff <- list(data = data, binary = bin)
    if (!is.null(order)) suff$order <- order
  } else suff <- list(data = data)
  suff$otherArgs <- list(...)
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


#note: x, y, S are indexes of vars

#' @importFrom splines ns
gamTestEachDir <- function(x, y, S, suffStat) {
#  print("testing")
#   browser()

  #Settings
  args <- suffStat$otherArgs
  if (!is.null(args) && "df" %in% names(args)) {
    dfs <- args$df
  } else dfs <- 3

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
    if (dfs > 0) x <- paste("ns(", x, dfString, sep = "")
#    x <- paste("ns(", x, dfString, sep = "")
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

gamTest <- function(x, y, S, suffStat) {
  p1 <- gamTestEachDir(x, y, S, suffStat)
  p2 <- gamTestEachDir(y, x, S, suffStat)

  max(p1,p2)
}


#note: vnames can be dropped
tpdag <- function(skel, order, vnames = skel@graph@nodes) {
  tempSkelAmat <- orderRestrictAmat(amat(skel), order = order)
  skel@graph <- as(graph::ftM2adjM(as.matrix(edgesFromAdjMat(tempSkelAmat)),
                                   V = vnames),
                   "graphNEL")
  addBgKnowledge(vOrientTemporal(skel), checkInput = FALSE)
}



orderRestrictAmat <- function(amat, order) {
  p <- nrow(amat)
  vnames <- rownames(amat)

  for (i in 1:p) {
    for (j in 1:p) {
      if (is.after(vnames[i], vnames[j], order)) amat[j,i] <- 0
    }
  }
  amat
}






vOrientTemporal <- function(skel) {
  graph <- skel@graph
  amat <- amat(skel)
  sepsets <- skel@sepset
  vnames <- graph@nodes
  nvar <- nrow(amat)
  # browser()

  for (i in 1:nvar) {
    theseAdj <- which(vnames %in% graph::adj(graph, i)[[1]])

    #if there are at least two adjacent nodes
    if (length(theseAdj) >= 2) {

      adjpairs <- gtools::combinations(length(theseAdj), 2, v = theseAdj)

      npairs <- nrow(adjpairs)
      #browser()
      if (npairs > 1) {

        for (j in 1:npairs) {
          thisPair <- adjpairs[j,]
          j1 <- thisPair[1]
          j2 <- thisPair[2]
          thisPairAdj <- j2 %in% graph::adj(graph, j1)[[1]]

          #if pair is not adjacent (unmarried)
          if (!thisPairAdj) {

            sepset1 <- sepsets[[j1]][[j2]]
            sepset2 <- sepsets[[j2]][[j1]]

            #if middle node is not a seperator of two other nodes
            if (!(i %in% sepset1) & !(i %in% sepset2)) {

              #if this does not contradict directional information
              #already in the graph
              if (amat[j1,i] == 1 & amat[j2, i] == 1) {
                amat[i, j1] <- 0
                amat[i, j2] <- 0
              }
            }
          }
        }
      }
    }
  }
  amat
}



