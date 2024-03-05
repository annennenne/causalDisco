#Load mixed numerical and categorical dataset mixData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_mixData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using mmhc():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_mmhc_out <- mmhc(mixData)

#Print output
bnlearn_mmhc_out

#Plot output
plot(bnlearn_mmhc_out)

