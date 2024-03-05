#Load numerical dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using pc.stable():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_pcstable_out <- pc.stable(numData)

#Print output
bnlearn_pcstable_out

#Plot output
plot(bnlearn_pcstable_out)

