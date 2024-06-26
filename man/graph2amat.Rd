% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{graph2amat}
\alias{graph2amat}
\title{Convert graphNEL object to adjacency matrix}
\usage{
graph2amat(graph, toFrom = TRUE, type = "pdag")
}
\arguments{
\item{graph}{A graphNEL object.}

\item{toFrom}{Logical indicating whether the resulting adjancency
matrix is "to-from" (default), or "from-to", see details.}

\item{type}{The type of adjancency matrix, must be one of \code{"pdag"} or
\code{"ag"}. \code{"pdag"} should be used for directed graphs, namely
DAG, CPDAG, MPDAG, TPDAG and PDAG adjacency matrices, i.e. adjacency matrices
where A(i,j) = A(j,i) = 1 is interpreted as an undirected edge. \code{"ag"}
may be used for ADMGs, MAGs, PAGs and TPAGs, where further possible arrowhead
options are available (see \link{amat})}
}
\description{
Convert graphNEL object to adjacency matrix
}
\details{
A "to-from" \code{pdag} adjacency matrix is encoded as follows: A(i,j) = 1 and A(j,i) = 0
means there is an edge i -> j. A(j,i) = 1 and A(i,j) = 0 means there is an edge j -> i.
A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j.
A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j.

A "from-to" adjacency matrix is the transpose of a "to-from" adjacency matrix.
A "from-to" \code{pdag} adjacency matrix is hence encoded as follows: A(i,j) = 1 and A(j,i) = 0
means there is an edge j -> i. A(j,i) = 1 and A(i,j) = 0 means there is an edge i -> j.
A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j.
A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j.

See \link{amat} for details about how an \code{ag} adjacency matrix is encoded.
}
