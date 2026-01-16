#Load numerical dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(bnlearn)

#Learn causal structure using si.hiton.pc():
#Mandatory arguments: 
##The dataset as a data.frame
#Optional arguments that are necessary for structure learning:
##undirected = FALSE. Default option estimates only the skeleton (i.e. does not
###direct the edges). 
bnlearn_sihitonpc_out <- si.hiton.pc(numData, undirected = FALSE)

#Print output
bnlearn_sihitonpc_out

#Plot output
plot(bnlearn_sihitonpc_out)

