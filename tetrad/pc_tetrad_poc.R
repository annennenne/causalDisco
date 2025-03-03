source("/home/fabben/BioStat/py-tetrad/pytetrad/R/my_functions.R")
library(rJava)

.jinit(parameters = "-Xmx2g")
.jaddClassPath(
  "/home/fabben/BioStat/py-tetrad/pytetrad/resources/tetrad-current.jar"
)
.jclassPath()


# data <- read.table("pytetrad/resources/airfoil-self-noise.continuous.txt", header=TRUE)
data <- read.table(
  "~/BioStat/py-tetrad/pytetrad/resources/example_sim_100-6-1000.txt",
  header = TRUE
)

## The read.table function will read decimal columns as real ('numeric')
## and integer columns as discrete. When passing data from R into Python,
## integer columns will still be interpreted as discrete, so we have to
## specify in the data frame for this data in columns 1-5 are to be interpreted
## as continuous (i.e., 'numeric'); some of them are integer columns.
i <- c(1, ncol(data))
data[, i] <- apply(data[, i], 2, function(x) as.numeric(x))
vars <- create_variables(data)

# This web site should how to pass a matrix as a double[][] array.
# https://www.rforge.net/rJava/docs/reference/jarray.html

# Get the sample size
sample_size <- nrow(data)

cov <- .jnew(
  "edu/cmu/tetrad/data/CovarianceMatrix",
  vars,
  .jarray(as.matrix(data), dispatch = TRUE),
  as.integer(sample_size)
)

## Use cov to make a SemBicScore...

score <- .jnew(
  "edu.cmu.tetrad.search.score.SemBicScore",
  .jcast(cov, "edu.cmu.tetrad.data.ICovarianceMatrix")
)
.jcall(score, "V", "setPenaltyDiscount", 2)
print(score)
## Construct a BOSS search and return a Tetrad Graph object.

# suborder_search <- .jnew("edu.cmu.tetrad.search.Pc",
# .jcast(score, "edu.cmu.tetrad.search.score.Score"))
perm_search <- .jnew(
  "edu.cmu.tetrad.search.PermutationSearch",
  .jcast(suborder_search, "edu.cmu.tetrad.search.SuborderSearch")
)
graph <- .jcall(perm_search, "Ledu/cmu/tetrad/graph/Graph;", "search", FALSE)

## Convert the graph to DOT format and display it.
dot <- .jcall(
  "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
  "Ljava/lang/String;",
  "graphToDot",
  graph
)
library('DiagrammeR')
grViz(dot)

.jmethods("edu.cmu.tetrad.search.Pc")
