##################################################
### Reference classes used for tges
##################################################

## Define TemporalBIC score

#' Temporal Bayesian Information Criterion (Score criterion)
#'
#'A Reference Class for Gaussian Observational Data Scoring with Tiered Background Knowledge. This class represents a score for causal discovery using tiered background knowledge from observational Gaussian
#' data; it is used in the causal discovery function \code{\link{tges}}.
#'
#' The class implements a score which scores all edges contradicting the ordering
#' (edge going from a later tier to an earlier) to minus \eqn{\infty}{∞}. If the
#' the edges does not contradict, the score is equal to that of [pcalg::GaussL0penObsScore-class]:
#' The class implements an \eqn{\ell_0}{ℓ0}-penalized Gaussian maximum
#' likelihood estimator. The penalization is a constant (specified by
#' the argument \code{lambda} in the constructor) times the number of
#' parameters of the DAG model. By default, the constant \eqn{\lambda}{λ} is
#' chosen as \eqn{\log(n)/2}{log(n)/2}, which corresponds to the BIC score.
#'
#' @section Extends:
#' Class [pcalg::GaussL0penObsScore-class] directly.
#'
#' All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
#'
#' @section Constructor:
#' \preformatted{
#' new("TemporalBIC",
#'   data = matrix(1, 1, 1),
#'   order =  rep(1,ncol(data)),
#'   lambda = 0.5 * log(nrow(data)),
#'   intercept = TRUE,
#'   ...)
#' }
#'
#' @param data A numeric matrix with \eqn{n} rows and \eqn{p} columns. Each row
#' corresponds to one observational realization.
#' @param order A vector specifying the order each variable. Can be either a vector of integers
#' or an vector of prefixes. If integers, such that the ith entry
#' will detail the order of the ith variable in the dataset. Must start at 1 an increase
#' with increments of 1. If prefixes, must be in order.
#' @param lambda Penalization constant (see details).
#' @param intercept Logical; indicates whether an intercept is allowed in the
#' linear structural equations (i.e., whether a nonzero mean is allowed).
#'
#' @author Tobias Ellegaard Larsen
#'
#' @examples
#' #Simulate Gaussian data
#' set.seed(123)
#' n <- 500
#' child_x <- rnorm(n)
#' child_y <- 0.5*child_x + rnorm(n)
#' child_z <- 2*child_x + child_y  + rnorm(n)
#'
#' adult_x <- child_x + rnorm(n)
#' adult_z <- child_z + rnorm(n)
#' adult_w <- 2*adult_z + rnorm(n)
#' adult_y <- 2*child_x + adult_w + rnorm(n)
#'
#' simdata <- data.frame(child_x, child_y, child_z,
#'                      adult_x, adult_z, adult_w,
#'                       adult_y)
#'
#' # Define order in prefix way
#' prefix_order <- c("child", "adult")
#'
#' # Define TBIC score
#' t_score <- new("TemporalBIC", order = prefix_order
#'                , data = simdata)
#' # Run tges
#' tges_pre <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_pre)
#'
#' # Define order in integer way
#' integer_order <- c(1,1,1,2,2,2,2)
#'
#' # Define TBIC score
#' t_score <- new("TemporalBIC", order = integer_order
#'                , data = simdata)
#' # Run tges
#' tges_int <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_int)
#'
#' @seealso
#' \code{\link{tges}}
#'
#' @export

#' @importClassesFrom pcalg GaussL0penIntScore
setRefClass(
  "TemporalBIC",
  contains = "GaussL0penIntScore",

  fields = list(
    .order = "vector"
  ),

  methods = list(
    initialize = function(
      data = matrix(1, 1, 1),
      nodes = colnames(data),
      lambda = 0.5 * log(nrow(data)),
      intercept = TRUE,
      format = c("raw", "scatter"),
      use.cpp = FALSE,
      order = rep(1, ncol(data)),
      ...
    ) {
      .order <<- order
      callSuper(
        data = data,
        targets = list(integer(0)),
        target.index = rep(as.integer(1), nrow(data)),
        nodes = nodes,
        lambda = lambda,
        intercept = intercept,
        format = format,
        use.cpp = use.cpp,
        ...
      )
    },

    local.score = function(vertex, parents, ...) {
      ## Check validity of arguments
      validate.vertex(vertex)
      validate.parents(parents)
      order <- .order
      if (order[vertex] >= max(c(order[parents], -Inf))) {
        #Checks if the tier of parents are before or same as node

        if (c.fcn == "none") {
          ## Calculate score in R
          if (.format == "raw") {
            ## calculate score from raw data matrix
            ## Response vector for linear regression
            Y <- pp.dat$data[pp.dat$non.int[[vertex]], vertex]
            sigma2 <- sum(Y^2)

            if (length(parents) + pp.dat$intercept != 0) {
              ## Get data matrix on which linear regression is based
              Z <- pp.dat$data[pp.dat$non.int[[vertex]], parents, drop = FALSE]
              if (pp.dat$intercept) {
                Z <- cbind(1, Z)
              }

              ## Calculate the scaled error covariance using QR decomposition
              Q <- qr.Q(qr(Z))
              sigma2 <- sigma2 - sum((Y %*% Q)^2)
            }
          } else if (.format == "scatter") {
            ## Calculate the score based on pre-calculated scatter matrices
            ## If an intercept is allowed, add a fake parent node
            parents <- sort(parents)
            if (pp.dat$intercept) {
              parents <- c(pp.dat$vertex.count + 1, parents)
            }

            pd.scMat <- pp.dat$scatter[[pp.dat$scatter.index[vertex]]]
            sigma2 <- pd.scMat[vertex, vertex]
            if (length(parents) != 0) {
              b <- pd.scMat[vertex, parents]
              sigma2 <- sigma2 -
                as.numeric(b %*% solve(pd.scMat[parents, parents], b))
            }
          }

          ## Return local score
          lscore <- -0.5 *
            pp.dat$data.count[vertex] *
            (1 + log(sigma2 / pp.dat$data.count[vertex])) -
            pp.dat$lambda * (1 + length(parents))
          return(lscore)
        } else {
          ## Calculate score with the C++ library (NOT ABLE TO DO THIS YET)
          stop("Not able to compute using C++. Set use.cpp = FALSE")
        }
      } else {
        skip <- -Inf
        return(skip)
      } #set score to minus infinity if vertex earlier than parents
    }
  ),
  inheritPackage = TRUE
)

# Define Temporal BDeu

#' Temporal Bayesian Dirichlet equivalent uniform (Score criterion)
#'
#' A reference class for categorical observational data Scoring with Tiered Background Knowledge. This class represents a score for causal discovery using tiered background knowledge from observational categorical
#' data; it is used in the causal discovery function \code{\link{tges}}.
#'
#' The class implements a score which scores all edges contradicting the ordering
#' (edge going from a later tier to an earlier) to minus \eqn{\infty}{∞}. If the
#' the edges does not contradict, the score is equal to that of the standard BDeu.
#'
#' @section Extends:
#' Class [pcalg::Score-class] directly.
#'
#' All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
#'
#' @section Constructor:
#' \preformatted{
#' new("TemporalBdeu",
#'   data = matrix(1, 1, 1),
#'   order =  rep(1,ncol(data)),
#'   iss = 1
#'   ...)
#' }
#'
#'
#'
#' @param data A numeric matrix with \eqn{n} rows and \eqn{p} columns. Each row
#' corresponds to one observational realization.
#' @param order A vector specifying the order each variable. Can be either a vector of integers
#' or an vector of prefixes. If integers, such that the ith entry
#' will detail the order of the ith variable in the dataset. Must start at 1 an increase
#' with increments of 1. If prefixes, must be in order.
#' @param iss Imaginary Sample Size (ISS), also referred to as
#' Equivalent Sample Size (ESS), determines how much weight is assigned to the prior
#' in terms of the size of animaginary sample supporting it. Increasing the ISS will
#' increase the density of the estimated graph.
#'
#' @author Tobias Ellegaard Larsen
#'
#' @examples
#' # For reproducibility
#' set.seed(123)
#'
#' # Number of samples
#' n <- 1000
#'
#' # Define probabilities for A
#' p_A <- c(0.4, 0.35, 0.25)  # Probabilities for A = {1, 2, 3}
#'
#' # Simulate A from a categorical distribution
#' A <- sample(1:3, n, replace = TRUE, prob = p_A)
#'
#' # Define conditional probabilities for B given A
#' p_B_given_A <- list(
#'   c(0.7, 0.3),  # P(B | A=1)
#'   c(0.4, 0.6),  # P(B | A=2)
#'   c(0.2, 0.8)   # P(B | A=3)
#' )
#'
#' # Sample B based on A
#' B <- sapply(A, function(a) sample(1:2, 1, prob = p_B_given_A[[a]]))
#'
#' # Define conditional probabilities for C given A and B
#' p_C_given_A_B <- list(
#'   "1_1" = c(0.6, 0.4),  # P(C | A=1, B=1)
#'   "1_2" = c(0.3, 0.7),  # P(C | A=1, B=2)
#'   "2_1" = c(0.5, 0.5),  # P(C | A=2, B=1)
#'   "2_2" = c(0.2, 0.8),  # P(C | A=2, B=2)
#'   "3_1" = c(0.7, 0.3),  # P(C | A=3, B=1)
#'   "3_2" = c(0.4, 0.6)   # P(C | A=3, B=2)
#' )
#'
#' # Sample C based on A and B
#' C <- mapply(function(a, b) sample(1:2, 1, prob = p_C_given_A_B[[paste(a, b, sep = "_")]]), A, B)
#'
#' # Create dataset
#' simdata <- data.frame(as.factor(A), as.factor(B), as.factor(C))
#'
#' # Define order in prefix way
#' colnames(simdata) <- c("child_A","child_B","adult_C")
#' prefix_order <- c("child", "adult")
#'
#' # Define TemporalBDeu score
#' t_score <- new("TemporalBDeu", order = prefix_order
#'                , data = simdata)
#' # Run tges
#' tges_pre <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_pre)
#'
#'
#' # Define order in integer way
#' colnames(simdata) <- c("A","B","C")
#' integer_order <- c(1,1,2)
#'
#' # Define TemporalBDeu score
#' t_score <- new("TemporalBDeu", order = integer_order
#'                , data = simdata)
#' # Run tges
#' tges_integer <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_integer)
#'
#' @section Alternative implementation of TemporalBDeu score:
#' We provide here a faster alternative to the implemented version of TemporalBDeu.
#' However, this version relies on a non-exporten function from \pkg{bnlearn}.
#' We provide the code for it here:
#' ```r
#' setRefClass("TemporalBDeu",
#'             contains = "DataScore",
#'
#'             fields = list(
#'               .order = "vector",
#'               .iss = "numeric"),
#'
#'             methods = list(
#'               initialize = function(data = matrix(1, 1, 1),
#'                                     nodes = colnames(data),
#'                                     iss = 1,
#'                                     order = rep(1,ncol(data)),
#'                                     ...) {
#'                 .order <<- order
#'                 .iss <<- iss
#'                 callSuper(data = data,
#'                           nodes = nodes,
#'                           iss = iss,
#'                           ...)},
#'
#'
#'               local.score = function(vertex, parents,...) {
#'                 ## Check validity of arguments
#'                 validate.vertex(vertex)
#'                 validate.parents(parents)
#'                 order <- .order
#'                 iss <- .iss
#'                 if (order[vertex] >= max(c(order[parents],-Inf))){
#'                   #Checks if the tier of parents are before or same as node
#'
#'                   # Create local dataset
#'                   D <- pp.dat$data
#'                   pa_nam <- colnames(D)[parents]
#'                   ve_nam <- colnames(D)[vertex]
#'                   res_nam <- colnames(D)[-c(parents,vertex)]
#'
#'
#'                   # Create local bn object
#'                   if (length(parents) > 0){
#'                     mod_string <- paste(c("[",
#'                                           paste(c(pa_nam,res_nam),collapse = "]["),
#'                                           "][",
#'                                           ve_nam,
#'                                           "|",
#'                                           paste(pa_nam,collapse = ":"),
#'                                           "]"
#'                     ),collapse = "")
#'
#'                   } else {
#'                     mod_string <- paste(c("[",
#'                                           paste(c(pa_nam,res_nam),collapse = "]["),
#'                                           "][",
#'                                           ve_nam,
#'                                           "]"
#'                     ),collapse = "")
#'                   }
#'
#'                   bn_ob <- as.bn(mod_string)
#'                   BdeuScore <- bnlearn:::per.node.score(network = bn_ob,
#'                                                         data = D,
#'                                                         score = "bde",
#'                                                         targets = ve_nam,
#'                                                         extra.args = list(iss = iss,
#'                                                                           prior = "uniform"))
#'                   return(BdeuScore)
#'                 }
#'                 else { skip <- -Inf
#'                 return(skip)}#set score to minus infinity if vertex earlier than parents
#'               }),
#'             inheritPackage = TRUE
#' )
#' ```
#'
#' @seealso
#' \code{\link{tges}}
#'
#' @export

setRefClass(
  "TemporalBDeu",
  contains = "DataScore",

  fields = list(
    .order = "vector",
    .iss = "numeric"
  ),

  methods = list(
    initialize = function(
      data = matrix(1, 1, 1),
      nodes = colnames(data),
      iss = 1,
      order = rep(1, ncol(data)),
      ...
    ) {
      .order <<- order
      .iss <<- iss
      callSuper(data = data, nodes = nodes, iss = iss, ...)
    },

    local.score = function(vertex, parents, ...) {
      ## Check validity of arguments
      validate.vertex(vertex)
      validate.parents(parents)
      order <- .order
      iss <- .iss
      if (order[vertex] >= max(c(order[parents], -Inf))) {
        #Checks if the tier of parents are before or same as node

        # Create local dataset
        D <- pp.dat$data[, c(vertex, parents), drop = FALSE]
        pa_nam <- colnames(pp.dat$data)[parents]
        ve_nam <- colnames(pp.dat$data)[vertex]

        #n_j: number of counts for parent set equal to configuration j ( = j)
        #n_jk: number of counts for parent set equal to configuration j ( = j)
        #      and vertex state equal to state k ( = k)

        if (length(parents) == 0) {
          #q: number of possible configurations of states of parents
          q <- 1

          #r: number of states for vertex
          r <- nlevels(D[[ve_nam]])

          alpha_j <- iss

          # Set pa_score to 0
          pa_score <- lgamma(alpha_j) - lgamma(alpha_j + nrow(D))
        } else {
          #Number of in variables
          nlev_D <- sapply(D[, c(ve_nam, pa_nam)], nlevels)

          #q: number of possible configurations of states of parents
          q <- prod(nlev_D[pa_nam])

          #r: number of states for vertex
          r <- nlev_D[ve_nam]

          #Table with number of occurences for parents and parents combinations
          tab_pa <- as.data.frame(table(D[, c(pa_nam)]))

          alpha_j <- iss / q

          pa_score <- nrow(tab_pa) *
            lgamma(alpha_j) -
            sum(sapply(tab_pa$Freq, function(x) lgamma(alpha_j + x)))
        }

        #Table with number of occurences and state combinations
        tab_D <- as.data.frame(table(D))

        #Uniform prior with alpha according to imaginary sample size (iss)
        alpha_jk <- alpha_j / r

        BdeuScore <- sum(sapply(tab_D$Freq, function(x) lgamma(alpha_jk + x))) -
          nrow(tab_D) * lgamma(alpha_jk) +
          pa_score

        return(BdeuScore)
      } else {
        skip <- -Inf
        return(skip)
      } #set score to minus infinity if vertex earlier than parents
    }
  ),
  inheritPackage = TRUE
)

##################################################
### Functions
##################################################

#' Estimate the restricted Markov equivalence class using Temporal Greedy Equivalence Search
#'
#' Perform causal discovery using the temporal greedy equivalence search algorithm.
#'
#' @param score tiered scoring object to be used. At the moment only scores supported re \code{\linkS4class{TemporalBIC}} and \code{\linkS4class{TemporalBDeu}}.
#' @param verbose	indicates whether debug output should be printed.
#'
#' @author Tobias Ellegaard Larsen
#'
#' @return \code{tges} returns a \code{tamat} object which is a matrix with a
#' "order" attribute (a character vector listing the temporal order of the variables in the adjacency matrix).
#'
#' @examples
#' #Simulate Gaussian data
#' set.seed(123)
#' n <- 500
#' child_x <- rnorm(n)
#' child_y <- 0.5*child_x + rnorm(n)
#' child_z <- 2*child_x + child_y  + rnorm(n)
#'
#' adult_x <- child_x + rnorm(n)
#' adult_z <- child_z + rnorm(n)
#' adult_w <- 2*adult_z + rnorm(n)
#' adult_y <- 2*child_x + adult_w + rnorm(n)
#'
#' simdata <- data.frame(child_x, child_y, child_z,
#'                      adult_x, adult_z, adult_w,
#'                       adult_y)
#'
#' # Define order in prefix way
#' prefix_order <- c("child", "adult")
#'
#' # Define TBIC score
#' t_score <- new("TemporalBIC", order = prefix_order
#'                , data = simdata)
#' # Run tges
#' tges_pre <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_pre)
#'
#' # Define order in integer way
#' integer_order <- c(1,1,1,2,2,2,2)
#'
#' # Define TBIC score
#' t_score <- new("TemporalBIC", order = integer_order
#'                , data = simdata)
#' # Run tges
#' tges_int <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_int)
#'
#' @seealso
#' \code{\linkS4class{TemporalBIC}}
#'
#' @export

tges <- function(score, verbose = FALSE) {
  stopifnot(
    "Score must be of type TemporalBIC or TemporalBDeu, the only score criterion suported by tges at the moment." = class(
      score
    )[1] %in%
      c("TemporalBDeu", "TemporalBIC")
  )
  stopifnot(
    "When using TemporalBDeu the data must be factors." = class(score)[1] ==
      "TemporalBIC" |
      all(sapply(score$pp.dat$data, is.factor))
  )
  stopifnot("Data must not contain missing values" = !anyNA(score$pp.dat$data))
  node.numbers <- 1:score$pp.dat$vertex.count
  essgraph <- new(
    "TEssGraph",
    nodes = as.character(node.numbers),
    score = score
  )
  Forbidden.edges <- essgraph$.in.edges #list of nodes all with integer(0) entry
  node.names <- score$.nodes
  num.bidir <- 0
  num.directed <- 0

  #if order is given as a ordering of prefixes, change to integers
  pre_true <- (!is.numeric(score$.order))
  if (pre_true) {
    prefix_order <- score$.order
    int_order <- rep(NA, score$pp.dat$vertex.count)
    for (i_o in node.numbers) {
      int_order[i_o] <- which(
        score$.order == strsplit(node.names[i_o], "_")[[1]][1]
      )
    }
    if (any(is.na(int_order))) {
      stop("order need to be either prefixes of variable names or integers.")
    }
    score$.order <- int_order
  }
  order <- score$.order

  for (n in node.numbers) {
    Forbidden.edges[[n]] <- node.numbers[order[n] < order]
  }

  cont <- TRUE
  while (cont) {
    cont <- FALSE

    #Forward phase
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("forward", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) {
        cont <- TRUE
      } else {
        break
      }

      for (i in names(tempstep[-1])) {
        #Run through the nodes that have been changed
        in.node.edges <- tempstep[-1][[i]] #save the in.node edges of node i
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges] #List of edges to be removed
        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(
            gInput = createAdjMatrixFromList(essgraph$.in.edges),
            x = bgx,
            y = bgy,
            verbose = verbose
          )
          no.forbidden.edges <- createListFromAdjMatrix(amatbg)
          essgraph$.in.edges <- no.forbidden.edges
        }
      }
    }

    #Backward phase
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("backward", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) {
        cont <- TRUE
      } else {
        break
      }

      for (i in names(tempstep[-1])) {
        #Run through the nodes that have been changed
        in.node.edges <- tempstep[-1][[i]] #save the in.node edges of node i
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges] #List of edges to be removed
        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(
            gInput = createAdjMatrixFromList(essgraph$.in.edges),
            x = bgx,
            y = bgy,
            verbose = verbose
          )
          no.forbidden.edges <- createListFromAdjMatrix(amatbg)
          essgraph$.in.edges <- no.forbidden.edges
        }
      }
    }

    #Turning phase
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("turning", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) {
        cont <- TRUE
      } else {
        break
      }

      for (i in names(tempstep[-1])) {
        #Run through the nodes that have been changed
        in.node.edges <- tempstep[-1][[i]] #save the in.node edges of node i
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges] #List of edges to be removed
        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(
            gInput = createAdjMatrixFromList(essgraph$.in.edges),
            x = bgx,
            y = bgy,
            verbose = verbose
          )
          no.forbidden.edges <- createListFromAdjMatrix(amatbg)
          essgraph$.in.edges <- no.forbidden.edges
        }
      }
    }
  }
  #essgraph$.nodes <- node.names # Save names of nodes
  #names(essgraph$.in.edges) <- node.names # Save names of nodes
  if (!pre_true) {
    node.names <- paste(
      paste("T", as.character(order), sep = ""),
      node.names,
      sep = "_"
    )
    prefix_order <- paste("T", as.character(1:max(order)), sep = "")
  }
  t_amat <- t(as(essgraph, "matrix")) * 1
  colnames(t_amat) <- rownames(t_amat) <- node.names
  return_object <- causalDisco::tamat(t_amat, order = prefix_order)

  if (verbose) {
    cat(
      "Number of edges directed",
      num.bidir,
      "\nNumber of directed edges removed",
      num.directed,
      "\n"
    )
  }
  #return(essgraph)
  return(return_object)
}


############################################################################
## Not exported below ######################################################
############################################################################

# Define functions

## transform list of type .in.edges to adjancency matrix

createAdjMatrixFromList <- function(inputList) {
  # Determine the number of rows and columns (n x n matrix)
  n <- length(inputList)

  # Initialize a matrix of 0s
  resultMatrix <- matrix(0, nrow = n, ncol = n)

  # Iterate over the list to update the matrix
  for (i in seq_along(inputList)) {
    indices <- inputList[[i]]
    # Ensure indices are within the bounds of the matrix
    validIndices <- indices[indices <= n]
    if (length(validIndices) > 0) {
      resultMatrix[i, validIndices] <- 1
    }
  }
  rownames(resultMatrix) <- names(inputList)
  colnames(resultMatrix) <- names(inputList)
  return(resultMatrix)
}

## Adjancency matrix to .in.edges list for an essgraph

createListFromAdjMatrix <- function(adjMatrix) {
  # Get the number of rows (or columns, since it's n x n) in the adjacency matrix
  n <- nrow(adjMatrix)

  # Initialize an empty list
  resultList <- vector("list", n)
  names(resultList) <- rownames(adjMatrix)

  # Iterate over the rows of the matrix
  for (i in 1:n) {
    # Find the indices (column numbers) where there is an in edge (value == 1)
    connectedIndices <- as.integer(which(adjMatrix[i, ] == 1))

    if (length(connectedIndices) > 0) {
      resultList[[i]] <- connectedIndices
    } else {
      # If no connections, assign NULL or an empty vector
      resultList[[i]] <- integer(0)
    }
  }

  return(resultList)
}

# Define reference classes

## Define new EssGraph class (TEssgraph) with new greedy step that also return new edges added

TEssGraph <- setRefClass(
  "TEssGraph",
  contains = "EssGraph",
  methods = list(
    # Performs one greedy step
    greedy.step = function(
      direction = c("forward", "backward", "turning"),
      verbose = FALSE,
      ...
    ) {
      stopifnot(!is.null(score <- getScore()))

      ## Cast direction
      direction <- match.arg(direction)
      alg.name <- switch(
        direction,
        forward = "GIES-F",
        backward = "GIES-B",
        turning = "GIES-T"
      )

      new.graph <- .Call(
        causalInference,
        .in.edges,
        score$pp.dat,
        alg.name,
        score$c.fcn,
        causal.inf.options(
          caching = FALSE,
          maxSteps = 1,
          verbose = verbose,
          #adaptive = "none", #added by TOBIAS
          ...
        )
      )
      if (identical(new.graph, "interrupt")) {
        return(FALSE)
      }

      if (new.graph$steps > 0) {
        last.edges <- .in.edges
        .in.edges <<- new.graph$in.edges
        names(.in.edges) <<- .nodes

        new.in.edges <- .in.edges[sapply(names(.in.edges), function(x) {
          !identical(.in.edges[[x]], last.edges[[x]])
        })]
        #returns if any new edges have been added. (Also returns if an edge have been removed and
        #there still is an edge going in to this node.)
      } else {
        new.in.edges <- list()
      }

      return(c((new.graph$steps == 1), new.in.edges))
    }
  ),
  inheritPackage = TRUE
)
