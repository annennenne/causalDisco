#' Orient the edges of a skeleton 
#' 
#' 
#' @export
orient <- function(skeleton, method = "tpc", ...) {
  if (is.temposkeleton(skeleton) & is.null(method)) method <- "tpc"
  if (method == "tpc") {
    if (is.temposkeleton(skeleton)) {
      
    }
  }
}







############################################################################
## Not exported below ######################################################
############################################################################





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


#note: vnames can be dropped
tpcOrientEdges <- function(skel, order, vnames = skel@graph@nodes) {
  tempSkelAmat <- orderRestrictAmat(amat(skel), order = order)
  skel@graph <- as(graph::ftM2adjM(as.matrix(edgesFromAdjMat(tempSkelAmat)),
                                   V = vnames),
                   "graphNEL")
  addBgKnowledge(vOrientTemporal(skel), checkInput = FALSE)
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


