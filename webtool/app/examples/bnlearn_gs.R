#Load numerical dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using gs():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_gs_out <- gs(numData)

#Print output
bnlearn_gs_out

#Plot output
plot(bnlearn_gs_out)

