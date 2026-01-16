#Load numerical dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using inter.iamb():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_interiamb_out <- inter.iamb(numData)

#Print output
bnlearn_interiamb_out

#Plot output
plot(bnlearn_interiamb_out)

