a <- read.table("undiradj.txt", header = TRUE)

rtable <- data.frame(model = paste("m", 1:9, sep = ""),
                 nedges = rev(colSums(a))/2,
                 ndiff = c(                        sum(a$m1 != a$m2),
                           sum(a$m2 != a$m3),
                           sum(a$m3 != a$m4),
                           sum(a$m4 != a$m5),
                           sum(a$m5 != a$m6),
                           sum(a$m6 != a$m7),
                           sum(a$m7 != a$m8),
                           sum(a$m8 != a$m9), NA)/2,
                 nadded = c(sum(a$m1-a$m2 == 1),
                              sum(a$m2-a$m3 == 1),
                              sum(a$m3-a$m4 == 1),
                              sum(a$m4-a$m5 == 1),
                              sum(a$m5-a$m6 == 1),
                              sum(a$m6-a$m7 == 1),
                              sum(a$m7-a$m8 == 1),
                              sum(a$m8-a$m9 == 1),
                            NA)/2,
                 nremoved = c(sum(a$m1-a$m2 == -1),
                            sum(a$m2-a$m3 == -1),
                            sum(a$m3-a$m4 == -1),
                            sum(a$m4-a$m5 == -1),
                            sum(a$m5-a$m6 == -1),
                            sum(a$m6-a$m7 == -1),
                            sum(a$m7-a$m8 == -1),
                            sum(a$m8-a$m9 == -1),
                            NA)/2)

rtable$retention <- 100*(rtable$nedges -  rtable$nadded) / rtable$nedges

library(xtable)
xtable(rtable[9:1, -c(1,3)])


#########################################################################################
######## For replication code using package:
#########################################################################################

#using compare
source("./R/compare.R")

#with imported edge lists:
q <- rbind( unlist(compare(a$m9)[c("psi2", "nedges2", "nadded", "nremoved")]),
           unlist(compare(a$m9, a$m8)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m8, a$m7)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m7, a$m6)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m6, a$m5)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m5, a$m4)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m4, a$m3)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m3, a$m2)[c("psi2", "nedges2", "nadded", "nremoved")]),
      unlist(compare(a$m2, a$m1)[c("psi2", "nedges2", "nadded", "nremoved")])
)


#version with tpdags in list:
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
