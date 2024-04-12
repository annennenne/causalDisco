#' Plot temporal graph via Latex
#' 
#' Generates a plot of a tamat, tpdag or tpag object by use of Latex tikz. Note
#' that a working Latex installation is required. Note also that this function
#' is slower than typical R graphics options and may take some time to terminate. 
#' 
#' @details The function renders Latex code using rmarkdown, which relies on a working
#' installation of Latex. Afterwards, the resulting pdf graphic is loaded into
#' R and displayed in a browser. If working in Rstudio it may be opened in the built-in
#' viewer, depending on Rstudio global settings. 
#' 
#' @param x A tamat, tpdag, or tpag object as obtained from \code{\link{tamat}}, 
#' \code{\link{tpc}}, or \code{\link{tfci}}, respectively. 
#' @param filename Name of files that will be used internally during the function's
#' runtime. This filename will be appended with both .rmd and .pdf. 
#' Note that unless \code{keepfiles = TRUE},
#' these files will automatically be deleted again.
#' @param keepfiles If \code{FALSE} (default), temporary files used for making
#' the plot are deleted, otherwise they are kept and will be placed in the 
#' working directory. 
#' @param bendedges If \code{TRUE} (default), all edges are bent 10 degrees
#' to the right, thereby avoiding edges placed exactly on top of eachother.
#' @param ... Additional argument passed to \code{\link{maketikz}}.
#' 
#' @importFrom rmarkdown render
#' @importFrom magick image_read_pdf image_browse
#' @export 
tplot <- function(x, filename = "causaldisco_tplot_temp",
                  keepfiles = FALSE, bendedges = TRUE, ...) {
  headercode <- c("---",
                  "output: pdf_document",
                  "documentclass: standalone",
                  "classoption: \'border = 2pt\'",
                  "header-includes:",
                  " - \\usepackage{tikz}",
                  " - \\usetikzlibrary{arrows.meta,arrows,shapes,decorations,automata,backgrounds,petri}",
                  " - \\usepackage{pgfplots}",
          #        " - \\pgfplotsset{compat=1.18}",
                  "---")
  
  
  tcode <- maketikz(x, rawout = TRUE, bendedges = bendedges, ...)
  file <- paste(filename, ".rmd", sep = "")
  
  fileConn <- file(file, "w") 
  
  #wrap in try to ensure connection is closed in the end
  try({
    writer(headercode, outfile = fileConn)
    
    #remove first line of tikzcode (comment stating its made by causaldisco)
    writer(tcode[-1], outfile = fileConn)
  }) ## Now we should not write anything more to the file - End try.
  
  ## Force flush and close connection
  flush(fileConn)
  close(fileConn)
  
  try({
    suppressWarnings(render(file, quiet = TRUE))
  })
  
  try({
  img <- image_read_pdf(paste(filename, ".pdf", sep = "")) 
  })
  
  if (!keepfiles) {
    unlink(paste(filename, ".rmd", sep = ""))
    unlink(paste(filename, ".pdf", sep = ""))
  }

  image_browse(img)
}


############################################################################
## Not exported below ######################################################
############################################################################


writer <- function(x, ..., outfile, sep="\n") {
  cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
}

