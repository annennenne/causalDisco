#Load numeric dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(stablespec)


stablespec_out <- stableSpec(edata, longitudinal = FALSE)
stablespec_out_graph <- stablespec_out$graph

#look at incoming edges
stablespec_out_graph@edgeL

#Learn causal structure using stableSpec()
#Mandatory arguments: 
##A dataset
##longitudinal: Specify whether the data are longitudinal (TRUE) or 
###cross-sectional (FALSE). We choose FALSE. 
stablespec_out <- stableSpec(numData, longitudinal = FALSE)


#Look at outputted graph
  #choose graph object
  stablespec_out_graph <- stablespec_out$graph
  
  #Look at incoming edges (manually added variable names)
  sapply(stablespec_out_graph@edgeL, 
         function(x) names(numData)[x$edges]
  )

#No native plot method. But outputted object is compatible with Rgraphviz and
#renderGraph() may be used:
Rgraphviz::renderGraph(stablespec_out_graph)

