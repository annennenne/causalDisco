#Load mixed numerical and categorical dataset mixData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_mixData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using hc():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_hc_out <- hc(mixData)

#Print output
bnlearn_hc_out

#Plot output
plot(bnlearn_hc_out)

