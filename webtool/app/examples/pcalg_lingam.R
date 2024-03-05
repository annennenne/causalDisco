#Load numeric dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(pcalg)

#Prepare data for lingam() call: convert data into matrix
numData_matrix <- as.matrix(numData)

#Learn causal structure using lingam()
#Mandatory arguments: 
##The dataset in matrix format
pcalg_lingam_out <- lingam(numData_matrix)

#Outputted adjacency matrix
as(pcalg_lingam_out, "amat")

#No native plot method available.