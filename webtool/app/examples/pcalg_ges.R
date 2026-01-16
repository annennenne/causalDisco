#Load numeric dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(pcalg)

#Prepare data for ges() call: Define score object 
##Note: this choice of score is suitable for Gaussian data.
pcalg_score_numData <- new("GaussL0penObsScore", numData)


#Learn causal structure using ges()
#Mandatory arguments: 
##A score object containg the data
pcalg_ges_out <- ges(pcalg_score_numData)

#Look at ingoing edges for each variable for outputted 
#causal model
sapply(pcalg_ges_out$essgraph$field(".in.edges"), 
       function(x) names(numData[x]))

#Plot output
plot(pcalg_ges_out$essgraph)


