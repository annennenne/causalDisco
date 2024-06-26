% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maketikz.R
\name{maketikz}
\alias{maketikz}
\title{Generate Latex tikz code for plotting a temporal DAG, PDAG or PAG.}
\usage{
maketikz(
  model,
  xjit = 2,
  yjit = 2,
  markperiods = TRUE,
  xpgap = 4,
  annotateEdges = NULL,
  addAxis = TRUE,
  varLabels = NULL,
  periodLabels = NULL,
  annotationLabels = NULL,
  clipboard = TRUE,
  rawout = FALSE,
  colorAnnotate = NULL,
  bendedges = FALSE
)
}
\arguments{
\item{model}{\code{tpdag}, \code{tskeleton}, \code{tpag}, or \code{tamat} object to plot.}

\item{xjit}{How much should nodes within a period be jittered horizontally.}

\item{yjit}{Vertical distance between nodes within a period.}

\item{markperiods}{If \code{TRUE}, gray boxes are drawn behind each period.}

\item{xpgap}{Horizontal gap between different periods.}

\item{annotateEdges}{If \code{TRUE}, add a text annotation to edges. If \code{annotationlabels}
are supplied, these labels will be used. Otherwise, the value in the inputted adjacency matrix corresponding
to the edge will be used. Cannot be used for \code{tpag}
input objects (or \code{ag} amat types).}

\item{addAxis}{If \code{TRUE}, a horizontal axis with period labels are added.}

\item{varLabels}{Optional labels for nodes (variables). Should be given as a named list, where
the name is the variable name, and the entry is the label, e.g. \code{list(vname = "Label for vname")}.}

\item{periodLabels}{Optional labels for periods. Should be given as a named list, where
the name is the period name (as stored in the \code{tamat}), and the entry is the label,
e.g. \code{list(periodname = "Label for period")}.}

\item{annotationLabels}{Optional labels for edge annotations. Only used if \code{annotateEdges = TRUE}.
Should be given as a named list, where the name is the edge annotation (as stored in the \code{tamat}),
and the entry is the label, e.g. \code{list(h = "High")}.}

\item{clipboard}{If \code{TRUE}, the tikz code is not printed, but instead copied to the clipboard,
so it can easily be pasted into a Latex document.}

\item{rawout}{If \code{TRUE}, the tikz code is only returned as a character vector.}

\item{colorAnnotate}{Named list of colors to use to mark edge annotations instead of labels. This
overrules \code{annotateEdges} and both are not available at the same time. The list should be given with
annotations as names and colors as entries, e.g. \code{list(h = "blue")}. Cannot be used for \code{tpag}
input objects (or \code{ag} amat types).}

\item{bendedges}{If \code{TRUE}, all edges are bend 10 degrees to the right, thereby avoiding having edges exactly on
top of each other.}
}
\value{
Silently returns a character vector with lines of tikz code. The function
furthermore has a side-effect. If \code{clipboard = TRUE}, the side-effect is that the tikz
code is also copied to the clipboard. If \code{clipboard = FALSE}, the tikz code is instead printed
in the console.
}
\description{
Generate Latex tikz code for plotting a temporal DAG, PDAG or PAG.
}
\details{
Note that it is necessary to read in relevant tikz libraries in the
Latex preamble. The relevant lines of code are (depending a bit on parameter settings): \cr
\code{\\usepackage{tikz}} \cr
\code{\\usetikzlibrary{arrows.meta,arrows,shapes,decorations,automata,backgrounds,petri}} \cr
\code{\\usepackage{pgfplots}}
}
\examples{

# Make tikz figure code from tpdag, print code to screen
data(tpcExample)
tpdag1 <- tpc(tpcExample, order = c("child", "youth", "oldage"), sparsity = 0.01,
              test = corTest)
maketikz(tpdag1, clipboard = FALSE)

# Make tikz figure code from tamat, copy code to clipboard
thisdag <- simDAG(5)
rownames(thisdag) <- colnames(thisdag) <- c("child_x", "child_y",
                                            "child_z", "adult_x", 
                                            "adult_y")
thistamat <- tamat(thisdag, order = c("child", "adult"))        
\dontrun{
maketikz(thistamat)            
}   
              
}
