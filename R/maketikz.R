#' Generate Latex tikz code for plotting a temporal DAG, PDAG or PAG. 
#'
#' @details Note that it is necessary to read in relevant tikz libraries in the 
#' Latex preamble. The relevant lines of code are (depending a bit on parameter settings): \cr
#' \code{\\usepackage{tikz}} \cr
#' \code{\\usetikzlibrary{arrows.meta,arrows,shapes,decorations,automata,backgrounds,petri}} \cr
#' \code{\\usepackage{pgfplots}}
#'
#' @param model \code{tpdag}, \code{tskeleton}, \code{tpag}, or \code{tamat} object to plot. 
#' @param xjit How much should nodes within a period be jittered horizontally.
#' @param yjit Vertical distance between nodes within a period.
#' @param markperiods If \code{TRUE}, gray boxes are drawn behind each period. 
#' @param xpgap Horizontal gap between different periods. 
#' @param annotateEdges If \code{TRUE}, add a text annotation to edges. If \code{annotationlabels}
#' are supplied, these labels will be used. Otherwise, the value in the inputted adjacency matrix corresponding
#' to the edge will be used. Cannot be used for \code{tpag}
#' input objects (or \code{ag} amat types).
#' @param addAxis If \code{TRUE}, a horizontal axis with period labels are added. 
#' @param varLabels Optional labels for nodes (variables). Should be given as a named list, where 
#' the name is the variable name, and the entry is the label, e.g. \code{list(vname = "Label for vname")}. 
#' @param periodLabels Optional labels for periods. Should be given as a named list, where 
#' the name is the period name (as stored in the \code{tamat}), and the entry is the label, 
#' e.g. \code{list(periodname = "Label for period")}. 
#' @param annotationLabels Optional labels for edge annotations. Only used if \code{annotateEdges = TRUE}. 
#' Should be given as a named list, where the name is the edge annotation (as stored in the \code{tamat}), 
#' and the entry is the label, e.g. \code{list(h = "High")}. 
#' @param clipboard If \code{TRUE}, the tikz code is not printed, but instead copied to the clipboard,
#' so it can easily be pasted into a Latex document. 
#' @param rawout If \code{TRUE}, the tikz code is only returned as a character vector.  
#' @param colorAnnotate Named list of colors to use to mark edge annotations instead of labels. This 
#' overrules \code{annotateEdges} and both are not available at the same time. The list should be given with
#' annotations as names and colors as entries, e.g. \code{list(h = "blue")}. Cannot be used for \code{tpag}
#' input objects (or \code{ag} amat types).
#' @param bendedges If \code{TRUE}, all edges are bend 10 degrees to the right, thereby avoiding having edges exactly on
#' top of each other. 
#' 
#' @return Silently returns a character vector with lines of tikz code. The function 
#' furthermore has a side-effect. If \code{clipboard = TRUE}, the side-effect is that the tikz 
#' code is also copied to the clipboard. If \code{clipboard = FALSE}, the tikz code is instead printed  
#' in the console.
#' 
#' @examples 
#' 
#' # Make tikz figure code from tpdag, print code to screen
#' data(tpcExample)
#' tpdag1 <- tpc(tpcExample, order = c("child", "youth", "oldage"), sparsity = 0.01,
#'               test = corTest)
#' maketikz(tpdag1, clipboard = FALSE)
#' 
#' # Make tikz figure code from tamat, copy code to clipboard
#' thisdag <- simDAG(5)
#' rownames(thisdag) <- colnames(thisdag) <- c("child_x", "child_y",
#'                                             "child_z", "adult_x", 
#'                                             "adult_y")
#' thistamat <- tamat(thisdag, order = c("child", "adult"))        
#' \dontrun{
#' maketikz(thistamat)            
#' }   
#'               
#' @importFrom clipr write_clip
#' @export
maketikz <- function(model, xjit = 2, yjit = 2,
                       markperiods = TRUE, xpgap = 4,
                       annotateEdges = NULL,
                       addAxis = TRUE,
                       varLabels = NULL,
                       periodLabels = NULL,
                       annotationLabels = NULL,
                       clipboard = TRUE,
                       rawout = FALSE,
                       colorAnnotate = NULL,
                       bendedges = FALSE) {
  if ("tpdag" %in% class(model) | "tskeleton" %in% class(model) | 
      "tpag" %in% class(model)) {
    amat <- model$tamat
    order <-  attr(amat, "order")
  } else if ("tamat" %in% class(model)) {
    amat <- model
    order <- attr(model, "order")
  } else {
    stop("Input model must be of class tpdag, tskeleton or tamat")
  }
  
  istpag <- attr(amat, "tamat_type") == "ag"
  
  if (istpag | (!is.null(colorAnnotate) & is.null(annotateEdges))) {
    annotateEdges <- FALSE
  }
  
  if (is.null(annotateEdges)) {
    annotateEdges <- !all(amat %in% c(0,1))
  }
  
  bendstr <- " "
  if (bendedges) bendstr <- " [bend right=10] "
  
  vars <- colnames(amat)
  nvar <- length(vars)
  nperiod <- length(order)
  
  vparts <- strsplit(vars, split = "_")
  periods <- sapply(vparts, function(x) x[1])
  vnames <- sapply(vparts, function(x) paste(x[-1], collapse = "\\_"))
  neachperiod <- as.numeric(table(periods)[order])
  maxpvar <- max(neachperiod)[1]
  maxypos <- maxpvar * yjit
  
  if (is.null(varLabels)) {
    varLabels <- vnames
  } else {
    varLabels <- unlist(varLabels[rownames(amat)])
  }
  
  if (is.null(periodLabels)) {
    periodLabels <- order 
  }
  
  out <- c("%TIKZ FIG MADE BY CAUSALDISCO", "\\begin{tikzpicture}")
  
  out <- c(out,  paste("[every node/.style={font=\\small, align = center},",
                       "every edge/.append style={nodes={font=\\itshape\\scriptsize}}]"))
  if (annotateEdges & !is.null(annotationLabels)) {
    for (i in 1:length(annotationLabels)) {
      thisAnnotation <- names(annotationLabels)[i]
      thisLab <- annotationLabels[[i]]
      amat[amat == thisAnnotation] <- thisLab
    }
  }
  
  pcounters <- rep(1, nperiod)
  
  for (i in 1:nvar) {
    thisperiod <- periods[i]
    thisvname <- varLabels[i]
    thispno <- which(order == thisperiod)
    thisptotal <- sum(periods == thisperiod)
    
    xpos <- (thispno-1)*xpgap + (thispno-1)*xjit + (i %% 2) * xjit
    ydist <- floor(maxypos / neachperiod[thispno])
    ypos <- (maxypos - ((neachperiod[thispno] - 1) * ydist))/2 + 
      ydist * (pcounters[thispno]-1) 
    
    out <- c(out, paste("\\node (", i, ") at (", xpos, ",", ypos, ") {", 
                        thisvname, "};", sep = ""))
    
    if (addAxis & pcounters[thispno] == 1) {
      out <- c(out, paste("\\node at (", (thispno-1)*xpgap + (thispno-1)*xjit  + xjit/2, 
                          ",", "-0.5", ") {", 
                          periodLabels[thispno], "};", sep = ""))
    }
    
    #if (pcounter == thisptotal) {
    #  pcounter <- 1 
    #} else { 
    #  pcounter <- pcounter + 1
    #}
    pcounters[thispno] <- pcounters[thispno] + 1
  }
  
  allundir <- list()
  
  if (!istpag) {
    for (i in 1:nvar) {
      thesechildren <- which(amat[, i] != 0)
      theseparents <- which(amat[i, ] != 0)
      theseundir <- intersect(thesechildren, theseparents)
      
      thesetruechildren <- setdiff(thesechildren, theseundir) 
      
      theseundir <- theseundir[theseundir < i] #only store when smaller, avoids duplicates
      if (length(theseundir) > 0 ) {
        allundir[[i]] <- theseundir
      }
      if (length(thesetruechildren) > 0) {
        if (!annotateEdges & is.null(colorAnnotate)) {
          out <- c(out, paste("\\draw [->] (", i, ") edge", bendstr, "(", thesetruechildren, ");", sep = ""))
        } 
        if (annotateEdges) {
          #  browser()
          out <- c(out, paste("\\draw [->] (", i, ") edge", bendstr, "node [above,sloped] {", amat[thesetruechildren, i],
                              "} (", thesetruechildren, ");", sep = ""))
        }
        if (!is.null(colorAnnotate)) {
          out <- c(out, paste("\\draw [->, ", unlist(colorAnnotate[amat[thesetruechildren, i]]), 
                              "] (", i, ") edge", bendstr, "(", thesetruechildren, ");", sep = ""))
        }
      }
    }
    
    n_undir <- length(allundir)
    if (n_undir > 0) {
      for (i in 1:length(allundir)) {
        theseneigh <- allundir[[i]]
        if (length(theseneigh) > 0) {
          if (!annotateEdges) {
            out <- c(out, paste("\\draw [-] (", i, ") edge", bendstr, "(", theseneigh, ");", sep = ""))
          }
          if (annotateEdges) {
            out <- c(out, paste("\\draw [-] (", i, ") edge", bendstr, "node [above,sloped] {", amat[theseneigh, i],
                                "} (", theseneigh, ");", sep = ""))
          }
        }
      }
    }
  }
  if (istpag) {
    ahead_from <- c("{Circle[open]}", "<", "")
    ahead_to <- c("{Circle[open]}", ">", "")
    alledges <- edges_pag(amat)
    
    alledges$tikzedge <- paste(ahead_from[alledges$a1], ahead_to[alledges$a2], 
                               sep = "-")
    n_edges <- nrow(alledges)
    if (n_edges > 0) {
      for (i in 1:nrow(alledges)) {
        out <- c(out, paste("\\draw [", alledges$tikzedge[i],
                            "] (", alledges$n1[i], ") edge", bendstr, "(", alledges$n2[i], ");", 
                            sep = ""))
      }
    }
  }
  
  if (addAxis) {
    max_xpos <- (nperiod-1)*xpgap + xjit*nperiod
    out <- c(out, paste("\\draw [-] (-1,0) edge (", max_xpos + 1, ",0);", sep = ""))
  }
  
  if (markperiods) { 
    out <- c(out, "\\begin{pgfonlayer}{background}", 
             "\\filldraw [join=round,black!10]")
    
    for (j in 1:nperiod) {
      xpos_1 <- (j - 1) * xpgap + (j - 1) * xjit
      ypos_1 <- 0
      xpos_2 <- xpos_1 + xjit
      ypos_2 <- (maxpvar) * yjit 
      
      out <- c(out, paste("(", xpos_1, ",", ypos_1, ") rectangle (",
                          xpos_2, ",", ypos_2, ")", sep = ""))
    }
    out <- c(out, "; \\end{pgfonlayer}")
  }
  
  out <- c(out, "\\end{tikzpicture}")
  
  if (rawout) return(out)
  
  if (clipboard) {
    write_clip(out)
  } else {
    cat(paste(out, collapse = "\n"))
  }
  
  invisible(out)
}






############################################################################
## Not exported below ######################################################
############################################################################

  
edges_pag <- function(amat, usevnames = FALSE) {
  res <- data.frame(n1 = character(), a1 = character(), a2 = character(), 
                    n2 = character())
  p <- nrow(amat)
  vnames <- colnames(amat)
  useamat <- amat
  
  for (i in 1:p) {
    outgoing <- which(useamat[, i] != 0)
    nout <- length(outgoing)
    if (nout > 0) {
      for (j in outgoing) {
        thisn1 <- i 
        thisn2 <- j
        thisa1 <- useamat[i,j]
        thisa2 <- useamat[j,i]
        useamat[i,j] <- 0
        res <- rbind(res, data.frame(n1 = thisn1, a1 = thisa1, a2 = thisa2, n2 = thisn2)) 
      }
    }
  }
  if (usevnames) {
    res$n1 <- vnames[res$n1]
    res$n2 <- vnames[res$n2]
  }
  res
}
