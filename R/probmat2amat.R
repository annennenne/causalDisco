#' Convert a matrix of probabilities into an adjacency matrix
#' 
#' @details  Two methods for converting the probability matrix into an adjacency 
#' matrix are implemented. First, the cutoff-method (\code{method = "cutoff"}) simply
#' uses a threshold value and sets all values below that to zero in the outputted 
#' adjacency matrix. No checks are performed to ensure that the resulting 
#' matrix is a proper dag/pdag/cpdag adjacency matrix. Second, the backwards 
#' PC orientation method (\code{method = "bpco"}) first uses a cutoff, and then
#' sets further elements to zero until the resulting matrix can be converted into 
#' a proper adjacency matrix (using the graph criterion specified in the 
#' \code{graph_criterion} argument) by applying the PC algorithm orientation rules. 
#' See Petersen et al. 2022 for further details.    
#' 
#' @param probmat Square matrix of probabilities.
#' @param threshold Value between 0 and 1. Any probabilities lower than 
#' this value will be set to 0 (no arrowhead). 
#' @param method Either \code{"cutoff"} or \code{"bpco"}, see details.
#' @param keep_vnames If \code{TRUE}, variable names (provided as rownames in the input probmat)
#' will be preserved in the output.
#' @param graph_criterion Which criterion to check if the output graph fulfills for the bpco
#' method. Should be one of \code{"dag"}, \code{"pdag"} or \code{"cpdag"}  or
#' \code{NULL}. Choosing \code{NULL} (the default) puts no further restrictions on the output.
#' See \code{\link[pcalg]{isValidGraph}} for definitions. 
#' @param deletesym If \code{TRUE}, edges are deleted symmetrically in the bcpo method. This means that instead
#' of removing arrowheads (setting singular elements to 0), the procedure removes full edges (setting both potential 
#' arrowheads for the given edge to zero). This only makes a difference if the graph may include undirected edges,
#' which should be encoded as bidirected edges. 
#' 
#' @return A square matrix of probabilities (all entries in \[0,1\]). 
#' 
#' @references Petersen, Anne Helby, et al. "Causal discovery for observational sciences using supervised machine learning." 
#' arXiv preprint arXiv:2202.12813 (2022).
#' 
#' @importFrom pcalg isValidGraph
#' 
#' @examples 
#' #Make random probability matrix that can be 
#' #converted into adjancency matrix
#' pmat <- matrix(runif(25, 0, 1), 5, 5)
#' diag(pmat) <- 0
#' 
#' #Convert to adjacency matrix using cutoff-method (threshold = 0.5)
#' probmat2amat(pmat, threshold = 0.5)
#' 
#' #Convert to adjacency matrix using BPCO-method (threshold = 0.5)
#' probmat2amat(pmat, threshold = 0.5, method = "bpco")
#' 
#' @export
probmat2amat <- function(probmat, threshold, method = "cutoff",
                         keep_vnames = TRUE, graph_criterion = "pdag",
                         deletesym = FALSE) {
  if (keep_vnames) vnames <- rownames(probmat)
  
  p <- nrow(probmat)
  
  if (method == "cutoff") {
    out <- matrix(as.numeric(probmat >= threshold), p, p)
  } 
  
  
  if (method == "bpco") {
    probmat[probmat < threshold] <- 0
    ord <- order(probmat, decreasing = TRUE)
    ord <- ord[probmat[ord] > 0]
    n_nonzero <- length(ord)
    i <- 1
    
    new <- probmat2amat(probmat, threshold, "cutoff")
    ord <- rev(ord)
    new_is_valid <- is_cpdag(new)
    
    badmat <- t(matrix(1:(p^2), p, p))
    
    while(!new_is_valid & i <= n_nonzero) {
      deleteind <- ord[i]
      new[deleteind] <- 0
      if (deletesym) new[which(badmat == deleteind)] <- 0
      
      new_is_valid <- is_cpdag(new) 
      if (!new_is_valid) {
        trynew <- correspondingCpdag(new)
        if (is_cpdag(trynew)) {
          new <- trynew
          new_is_valid <- TRUE
        }
      }
      i <- i + 1
    }
    if (new_is_valid) out <- new
  }
  
  if (!isValidGraph(out, graph_criterion) & method != "cutoff") warning("result is not a valid graph!")
  
  
  if (keep_vnames) dimnames(out) <- list(vnames, vnames)
  out
} 




###############################################################################################################
# Not exported below###########################################################################################
###############################################################################################################

# Internal function from pcalg: pcalg:::correspondingCpdag
# Copied from version 2.7-5
#' @importFrom pcalg addBgKnowledge
correspondingCpdag <- function(amat) {
  patt <- getPattern(amat)
  corresp.cpdag <- addBgKnowledge(patt, checkInput = FALSE)
  corresp.cpdag
}

# Internal function from pcalg: pcalg:::getPattern
# Copied from version 2.7-5
getPattern <- function(amat) {
  tmp <- amat + t(amat)
  tmp[tmp == 2] <- 1
  for (i in 1:(length(tmp[1, ]) - 1)) for (j in (i + 1):length(tmp[1, 
  ])) {
    if ((amat[j, i] == 0) & (amat[i, j] == 0) & (i != j)) {
      possible.k <- which(amat[, i] != 0 & amat[i, ] == 
                            0)
      if (length(possible.k) != 0) {
        for (k in 1:length(possible.k)) {
          if ((amat[possible.k[k], j] == 1) & (amat[j, 
                                                    possible.k[k]] == 0)) {
            tmp[i, possible.k[k]] <- 0
            tmp[j, possible.k[k]] <- 0
          }
        }
      }
    }
  }
  tmp
}