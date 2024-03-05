#Load mixed numerical and categorical dataset mixData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_mixData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using rsmax2():
#Mandatory arguments: 
##The dataset as a data.frame
bnlearn_rsmax2_out <- rsmax2(mixData)

#Print output
bnlearn_rsmax2_out

#Plot output
plot(bnlearn_rsmax2_out)

