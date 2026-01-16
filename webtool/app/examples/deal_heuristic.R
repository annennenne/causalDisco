#Load mixed numeric and categorical dataset mixData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_mixData.rda"))

#Load package
library(deal)

#Prepare data for deal calls
  #Make empty network (input argument: the dataset)
  deal_net <- network(mixData)
  
  #Add prior distribution to the network
  deal_jointprior <- jointprior(deal_net)
  
  #Update distributions of parameteres by combining prior with data.
  deal_net_wpar <- getnetwork(learn(deal_net, mixData, deal_jointprior))
  

#Learn causal structure using heuristic()
#Mandatory arguments: 
##The three components calculated above: A network with posterior parameter 
###distributions, the original dataset and a network with priors. 
deal_heuristic_out <- heuristic(deal_net_wpar, mixData, deal_jointprior)


#Extract network with best score:
deal_heuristic_outnet <- deal_heuristic_out$nw

#Print chosen network
deal_heuristic_outnet

#Plot chosen network
plot(deal_heuristic_outnet)



