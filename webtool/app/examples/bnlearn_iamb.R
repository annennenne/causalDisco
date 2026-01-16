#Load numerical dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using iamb():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_iamb_out <- iamb(numData)

#Print output
bnlearn_iamb_out

#Plot output
plot(bnlearn_iamb_out)

