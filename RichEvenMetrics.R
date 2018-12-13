library(tidyverse)
library(codyn)

setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/")

dat<-read.csv("AllData_11Dec2018.csv")%>%
  select(-X)

#calc relative cover
totcov<-dat%>%
  group_by(block, site, plot, trt)%>%
  summarise(totcov=sum(cover))

relcov<-merge(totcov, dat, by=c("block", "site", "plot", "trt"))%>%
  mutate(relcov=(cover/totcov)*100)

data1<-relcov%>%
  mutate(unid=paste(block, site, plot, trt, sep="::"))%>%
  filter(totcov!=0)

rich_evar<-community_structure(data1, abundance.var = "relcov", replicate.var = "unid", metric = "Evar")

rich_eq<-community_structure(data1, abundance.var = "relcov", replicate.var = "unid", metric = "EQ")

rich_esimp<-community_structure(data1, abundance.var = "relcov", replicate.var = "unid", metric = "SimpsonEvenness")


rich_evenness<-rich_eq%>%
  left_join(rich_evar)%>%
  left_join(rich_esimp)%>%
  separate(unid, into=c("block", "site", "plot", "trt"), sep="::")

Nam<-

write.csv(rich_evenness, "community_metrics_12122018.csv", row.names=F)

cor<-rich_evar%>%
  separate(unid, into=c("block", "site", "plot", "trt"), sep="::")%>%
    group_by(block)%>%
  na.omit%>%
  summarize(r.val=cor.test(richness, Evar)$estimate)

pairs(rich_evenness[,5:8])

##doing with an average for each plot.
