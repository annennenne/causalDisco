#' Generate Latex tikz code for plotting a temporal DAG, PDAG or PAG.
#'
#' @details Note that it is necessary to read in relevant tikz libraries in the
#' Latex preamble. The relevant lines of code are (depending a bit on parameter settings): \cr
#' \code{\\usepackage{tikz}} \cr
#' \code{\\usetikzlibrary{arrows.meta,arrows,shapes,decorations,automata,backgrounds,petri}} \cr
#' \code{\\usepackage{pgfplots}}
#'
#' @param model \code{tpdag}, \code{tskeleton}, \code{tpag}, or \code{tamat} object to plot.
#' @param x_jit How much should nodes within a period be jittered horizontally.
#' @param y_jit Vertical distance between nodes within a period.
#' @param mark_periods If \code{TRUE}, gray boxes are drawn behind each period.
#' @param period_gap Horizontal gap between different periods.
#' @param annotate_edges If \code{TRUE}, add a text annotation to edges. If \code{annotation_labels}
#' are supplied, these labels will be used. Otherwise, the value in the inputted adjacency matrix corresponding
#' to the edge will be used. Cannot be used for \code{tpag}
#' input objects (or \code{ag} amat types).
#' @param add_axis If \code{TRUE}, a horizontal axis with period labels are added.
#' @param var_labels Optional labels for nodes (variables). Should be given as a named list, where
#' the name is the variable name, and the entry is the label, e.g. \code{list(vname = "Label for vname")}.
#' @param period_labels Optional labels for periods. Should be given as a named list, where
#' the name is the period name (as stored in the \code{tamat}), and the entry is the label,
#' e.g. \code{list(periodname = "Label for period")}.
#' @param annotation_labels Optional labels for edge annotations. Only used if \code{annotate_edges = TRUE}.
#' Should be given as a named list, where the name is the edge annotation (as stored in the \code{tamat}),
#' and the entry is the label, e.g. \code{list(h = "High")}.
#' @param clipboard If \code{TRUE}, the tikz code is not printed, but instead copied to the clipboard,
#' so it can easily be pasted into a Latex document.
#' @param raw_out If \code{TRUE}, the tikz code is only returned as a character vector.
#' @param color_annotate Named list of colors to use to mark edge annotations instead of labels. This
#' overrules \code{annotate_edges} and both are not available at the same time. The list should be given with
#' annotations as names and colors as entries, e.g. \code{list(h = "blue")}. Cannot be used for \code{tpag}
#' input objects (or \code{ag} amat types).
#' @param bend_edges If \code{TRUE}, all edges are bend 10 degrees to the right, thereby avoiding having edges exactly
#' on top of each other.
#'
#' @return Silently returns a character vector with lines of tikz code. The function
#' furthermore has a side-effect. If \code{clipboard = TRUE}, the side-effect is that the tikz
#' code is also copied to the clipboard. If \code{clipboard = FALSE}, the tikz code is instead printed
#' in the console.
#'
#' @examples
#'
#' # Make tikz figure code from tpdag, print code to screen
#' data(tpc_example)
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ tidyselect::starts_with("child"),
#'     youth ~ tidyselect::starts_with("youth"),
#'     oldage ~ tidyselect::starts_with("oldage")
#'   )
#' )
#'
#' tpdag1 <- tpc_run(tpc_example,
#'   kn,
#'   alpha = 0.01,
#'   test = cor_test,
#'   output = "tpdag"
#' )
#' maketikz(tpdag1, clipboard = FALSE)
#'
#' # Make tikz figure code from tamat, copy code to clipboard
#' thisdag <- sim_dag(5)
#' rownames(thisdag) <- colnames(thisdag) <- c(
#'   "child_x", "child_y",
#'   "child_z", "adult_x",
#'   "adult_y"
#' )
#' thistamat <- tamat(thisdag, order = c("child", "adult"))
#' \dontrun{
#' maketikz(thistamat)
#' }
#'
#' @export
maketikz <- function(
  model,
  x_jit = 2,
  y_jit = 2,
  mark_periods = TRUE,
  period_gap = 4,
  annotate_edges = NULL,
  add_axis = TRUE,
  var_labels = NULL,
  period_labels = NULL,
  annotation_labels = NULL,
  clipboard = TRUE,
  raw_out = FALSE,
  color_annotate = NULL,
  bend_edges = FALSE
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "clipr"
    ),
    function_name = "maketikz"
  )

  if (
    "tpdag" %in%
      class(model) ||
      "tskeleton" %in% class(model) ||
      "tpag" %in% class(model)
  ) {
    amat <- model$tamat
    order <- attr(amat, "order")
  } else if ("tamat" %in% class(model)) {
    amat <- model
    order <- attr(model, "order")
  } else if (is_knowledgeable_caugi(model)) {
    order <- get_tiers(model)
    stop("Currently knowledgeable_caugis are not supported")
  } else {
    stop("Input model must be of class tpdag, tskeleton or tamat")
  }

  istpag <- attr(amat, "tamat_type") == "ag"

  if (istpag || (!is.null(color_annotate) && is.null(annotate_edges))) {
    annotate_edges <- FALSE
  }

  if (is.null(annotate_edges)) {
    annotate_edges <- !all(amat %in% c(0, 1))
  }

  bendstr <- " "
  if (bend_edges) {
    bendstr <- " [bend right=10] "
  }

  vars <- colnames(amat)
  nvar <- length(vars)
  nperiod <- length(order)

  vparts <- strsplit(vars, split = "_")
  periods <- sapply(vparts, function(x) x[1])
  vnames <- sapply(vparts, function(x) paste(x[-1], collapse = "\\_"))
  neachperiod <- as.numeric(table(periods)[order])
  maxpvar <- max(neachperiod)[1]
  maxypos <- maxpvar * y_jit

  if (is.null(var_labels)) {
    var_labels <- vnames
  } else {
    var_labels <- unlist(var_labels[rownames(amat)])
  }

  if (is.null(period_labels)) {
    period_labels <- order
  }

  out <- c("%TIKZ FIG MADE BY CAUSALDISCO", "\\begin{tikzpicture}")

  out <- c(
    out,
    paste(
      "[every node/.style={font=\\small, align = center},",
      "every edge/.append style={nodes={font=\\itshape\\scriptsize}}]"
    )
  )
  if (annotate_edges && !is.null(annotation_labels)) {
    for (i in seq_along(annotation_labels)) {
      this_annotation <- names(annotation_labels)[i]
      this_lab <- annotation_labels[[i]]
      amat[amat == this_annotation] <- this_lab
    }
  }

  pcounters <- rep(1, nperiod)

  for (i in 1:nvar) {
    thisperiod <- periods[i]
    thisvname <- var_labels[i]
    thispno <- which(order == thisperiod)
    thispno <- which(order == thisperiod) # TODO: not used

    xpos <- (thispno - 1) *
      period_gap +
      (thispno - 1) * x_jit +
      (i %% 2) * x_jit
    ydist <- floor(maxypos / neachperiod[thispno])
    ypos <- (maxypos - ((neachperiod[thispno] - 1) * ydist)) /
      2 +
      ydist * (pcounters[thispno] - 1)

    out <- c(
      out,
      paste(
        "\\node (",
        i,
        ") at (",
        xpos,
        ",",
        ypos,
        ") {",
        thisvname,
        "};",
        sep = ""
      )
    )

    if (add_axis && pcounters[thispno] == 1) {
      out <- c(
        out,
        paste(
          "\\node at (",
          (thispno - 1) * period_gap + (thispno - 1) * x_jit + x_jit / 2,
          ",",
          "-0.5",
          ") {",
          period_labels[thispno],
          "};",
          sep = ""
        )
      )
    }

    # if (pcounter == thisptotal) {
    #  pcounter <- 1
    # } else {
    #  pcounter <- pcounter + 1
    # }
    pcounters[thispno] <- pcounters[thispno] + 1
  }

  allundir <- list()

  if (!istpag) {
    for (i in 1:nvar) {
      thesechildren <- which(amat[, i] != 0)
      theseparents <- which(amat[i, ] != 0)
      theseundir <- intersect(thesechildren, theseparents)

      thesetruechildren <- setdiff(thesechildren, theseundir)

      theseundir <- theseundir[theseundir < i] # only store when smaller, avoids duplicates
      if (length(theseundir) > 0) {
        allundir[[i]] <- theseundir
      }
      if (length(thesetruechildren) > 0) {
        if (!annotate_edges && is.null(color_annotate)) {
          out <- c(
            out,
            paste(
              "\\draw [->] (",
              i,
              ") edge",
              bendstr,
              "(",
              thesetruechildren,
              ");",
              sep = ""
            )
          )
        }
        if (annotate_edges) {
          out <- c(
            out,
            paste(
              "\\draw [->] (",
              i,
              ") edge",
              bendstr,
              "node [above,sloped] {",
              amat[thesetruechildren, i],
              "} (",
              thesetruechildren,
              ");",
              sep = ""
            )
          )
        }
        if (!is.null(color_annotate)) {
          out <- c(
            out,
            paste(
              "\\draw [->, ",
              unlist(color_annotate[amat[thesetruechildren, i]]),
              "] (",
              i,
              ") edge",
              bendstr,
              "(",
              thesetruechildren,
              ");",
              sep = ""
            )
          )
        }
      }
    }

    for (i in seq_along(allundir)) {
      theseneigh <- allundir[[i]]

      if (length(theseneigh) == 0) {
        next
      }

      if (!annotate_edges) {
        out <- c(
          out,
          paste0(
            "\\draw [-] (",
            i,
            ") edge",
            bendstr,
            "(",
            theseneigh,
            ");"
          )
        )
      } else {
        out <- c(
          out,
          paste0(
            "\\draw [-] (",
            i,
            ") edge",
            bendstr,
            "node [above,sloped] {",
            amat[theseneigh, i],
            "} (",
            theseneigh,
            ");"
          )
        )
      }
    }
  }
  if (istpag) {
    ahead_from <- c("{Circle[open]}", "<", "")
    ahead_to <- c("{Circle[open]}", ">", "")
    alledges <- edges_pag(amat)

    alledges$tikzedge <- paste(
      ahead_from[alledges$a1],
      ahead_to[alledges$a2],
      sep = "-"
    )
    for (i in seq_len(nrow(alledges))) {
      out <- c(
        out,
        paste0(
          "\\draw [",
          alledges$tikzedge[i],
          "] (",
          alledges$n1[i],
          ") edge",
          bendstr,
          "(",
          alledges$n2[i],
          ");"
        )
      )
    }
  }

  if (add_axis) {
    max_xpos <- (nperiod - 1) * period_gap + x_jit * nperiod
    out <- c(
      out,
      paste("\\draw [-] (-1,0) edge (", max_xpos + 1, ",0);", sep = "")
    )
  }

  if (mark_periods) {
    out <- c(
      out,
      "\\begin{pgfonlayer}{background}",
      "\\filldraw [join=round,black!10]"
    )

    for (j in 1:nperiod) {
      xpos_1 <- (j - 1) * period_gap + (j - 1) * x_jit
      ypos_1 <- 0
      xpos_2 <- xpos_1 + x_jit
      ypos_2 <- (maxpvar) * y_jit

      out <- c(
        out,
        paste(
          "(",
          xpos_1,
          ",",
          ypos_1,
          ") rectangle (",
          xpos_2,
          ",",
          ypos_2,
          ")",
          sep = ""
        )
      )
    }
    out <- c(out, "; \\end{pgfonlayer}")
  }

  out <- c(out, "\\end{tikzpicture}")

  if (raw_out) {
    return(out)
  }

  if (clipboard) {
    clipr::write_clip(out)
  } else {
    cat(paste(out, collapse = "\n"))
  }

  invisible(out)
}


############################################################################
## Not exported below ######################################################
############################################################################

edges_pag <- function(amat, usevnames = FALSE) {
  res <- data.frame(
    n1 = character(),
    a1 = character(),
    a2 = character(),
    n2 = character()
  )
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
        thisa1 <- useamat[i, j]
        thisa2 <- useamat[j, i]
        useamat[i, j] <- 0
        res <- rbind(
          res,
          data.frame(n1 = thisn1, a1 = thisa1, a2 = thisa2, n2 = thisn2)
        )
      }
    }
  }
  if (usevnames) {
    res$n1 <- vnames[res$n1]
    res$n2 <- vnames[res$n2]
  }
  res
}
