#Load numerical dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using fast.iamb():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_fastiamb_out <- fast.iamb(numData)

#Print output
bnlearn_fastiamb_out

#Plot output
plot(bnlearn_fastiamb_out)

