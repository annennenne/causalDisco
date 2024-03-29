% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{FDR}
\alias{FDR}
\title{False Discovery Rate}
\usage{
FDR(confusion)
}
\arguments{
\item{confusion}{Confusion matrix as obtained from \link{confusion}}
}
\value{
A numeric in [0,1].
}
\description{
Computes false discovery rate from a confusion matrix, see \link{confusion}.
False discovery rate is defined as FP/(FP + TP), where FP are false positives and
TP are true positives. If FP + TP = 0, 0 is returned.
}
