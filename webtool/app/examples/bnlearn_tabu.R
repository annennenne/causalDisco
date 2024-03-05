#Load mixed numerical and categorical dataset mixData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_mixData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using tabu():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_tabu_out <- tabu(mixData)

#Print output
bnlearn_tabu_out

#Plot output
plot(bnlearn_tabu_out)

