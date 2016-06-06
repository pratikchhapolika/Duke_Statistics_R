library(ggplot2)
library(plyr)#Used for the revalue function
library(dplyr)

library(statsr)


load("./Inferential/project/gss.Rdata")

varsOfInterest <- c("year", "age", "sex", "race", "degree", "incom16", "relig", "attend", "class", "natspac")
modData <- select(gss, one_of(varsOfInterest))

countNAs <- apply(modData, 2, function(x) sum(is.na(x)))
countNAs#25834

modData2 <- select(modData, attend, natspac)
modData2 <- modData2 %>% filter(attend != "NA", natspac != "NA")

table(modData2$attend, modData2$natspac)

n_distinct(modData$attend)
levels(modData2$attend)

modData2$attend <- revalue(modData2$attend, c("Never"="Rarely", "Lt Once A Year"="Rarely", "Once A Year"="Rarely", "Sevrl Times A Yr"="Rarely"))
modData2$attend <- revalue(modData2$attend, c("Once A Month"="Frequently", "2-3X A Month"="Frequently", "Nrly Every Week"="Frequently", 
                                              "Every Week"="Frequently", "More Thn Once Wk"="Frequently"))
