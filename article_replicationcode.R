

















############################################################################
## Table 1 #################################################################
############################################################################

usevals <- c("psi2", "nedges2", "nadded", "nremoved")
table1 <- rbind(unlist(compare(dep_ms[[9]])),
                unlist(compare(dep_ms[[9]], dep_ms[[8]])[usevals]),
                unlist(compare(dep_ms[[8]], dep_ms[[7]])[usevals]),
                unlist(compare(dep_ms[[7]], dep_ms[[6]])[usevals]),
                unlist(compare(dep_ms[[6]], dep_ms[[5]])[usevals]),
                unlist(compare(dep_ms[[5]], dep_ms[[4]])[usevals]),
                unlist(compare(dep_ms[[4]], dep_ms[[3]])[usevals]),
                unlist(compare(dep_ms[[3]], dep_ms[[2]])[usevals]),
                unlist(compare(dep_ms[[2]], dep_ms[[1]])[usevals]))
table1 <- as.data.frame(table1)
names(table1) <- c("psi", "nedges", "nadded", "nremoved")
table1$retention <- 100*(table1$nedges -  table1$nadded) / table1$nedges
table1

xtable::xtable(table1)