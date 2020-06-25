

#####################################################################################################
#Helper functions for bnlearn
#####################################################################################################

#make blacklist from all  variables in fromvar to all variables in
#tovar. If either is NULL, then it is assumed that all variables but
#the specified block should be used
makeBlockedBL <- function(data, fromvar = NULL, tovar = NULL)  {
  allvar <- names(data)
  if(is.null(fromvar)) {
    fromvar <- setdiff(allvar, tovar)
  } else if (is.null(tovar)) {
    tovar <- setdiff(allvar, fromvar)
  }
  outframe <- data.frame(from = rep(fromvar, each = length(tovar)),
                         to = rep(tovar, length(fromvar)),
                         stringsAsFactors = FALSE)
  outframe <- outframe[!(outframe$from == outframe$to),]
  outframe
}

#make a blacklist from ordering
orderedBL <- function(data, order, sep = "_") {
  outBL <- data.frame(from = NULL, to = NULL)
  nPfs <- length(order)
  for (i in 2:nPfs) {
    fromvar <- getvar(data, order[i], sep = sep)
    tovar <- NULL
    for (j in 1:(i-1)) {
      tovar <- c(tovar, getvar(data, order[j]))
    }
    outBL <- rbind(outBL, makeBlockedBL(data, from = fromvar, to = tovar))
  }
  outBL
}


disco_varyalpha <- function(data, order, startAlpha,
                           mName,
                           labels = NULL,
                           maxModels = 10,
                           quiet = FALSE,
                           folder = getwd(),
                           procedure = "mmhc") {
  #browser()
  BL <- orderedBL(data, order)

  allModels <- list()
  allAlphas <- numeric()
  allNtests <- numeric()
  thisAlpha <- startAlpha
  nModels <- 0
  done <- FALSE
  while (!done) {
    useargs <- list(x = data, blacklist = BL, restrict.args = list(alpha = thisAlpha))
    if (procedure == "pc.stable") {
      useargs <- list(x = data, blacklist = BL, alpha = thisAlpha)
    }
    thisModel <-  do.call(procedure, useargs)
    thisNTest <- thisModel$learning$ntests
    allModels <- c(allModels, thisModel)
    allAlphas <- c(allAlphas, thisAlpha)
    allNtests <- c(allNtests, thisNTest)

    png(paste(folder, "/graph_", procedure, "_", mName, "_alpha", thisAlpha, ".png", sep = ""),
        width = 1000, height = 800, pointsize = 14)
    plotOrderedBN(thisModel, data, order,
                  vertex.label = labels,
                  asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
                  colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
                  sub = paste("alpha = ", thisAlpha, sep = ""))
    dev.off()

    nModels <- nModels + 1
    if (nModels >= maxModels) done <- TRUE

    nEdges <- nrow(arcs(thisModel))
    if (!quiet) print(paste("Finished model with alpha = ", thisAlpha,
                            ". Number of edges was ", nEdges,
                            ". Number of tests was ",
                            thisNTest, ".",
                            sep = ""))

    if (nEdges > 0) {
      thisAlpha <- thisAlpha/10
    } else done <- TRUE
  }
  list(models = allModels, alphas = allAlphas, ntests = allNtests)
}


mmhc_varyalpha <- function(...) {
  disco_varyalpha(..., procedure = "mmhc")
}
