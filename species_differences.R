library(tidyverse)
library(codyn)
library(gridExtra)

setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/")

theme_set(theme_bw(12))

rac <- read.csv('single_racs_climate_Dec2018.csv')

rac2<-rac%>%
  filter(block_trt!="NAmerica"&block_trt!='SAfrica')


bt<-unique(rac2$block_trt)

spdiff<-data.frame()

#first do species Differnece looping through the blocks
for (i in 1:length(bt)){
  
  subset<-rac2%>%
    filter(block_trt==bt[i])
  
  sp<-RAC_difference(subset, species.var = "species", abundance.var = "ave_relcov", replicate.var = "site")
  
  sp2<-sp%>%
    mutate(block_trt=bt[i])
  
  spdiff<-rbind(spdiff, sp2)
}

##add climate data
climate<-rac%>%
  select(block_trt, site, datAI)%>%
  unique()

climate2<-rac%>%
  select(block_trt, site, datAI)%>%
  unique()%>%
  rename(datAI2=datAI)%>%
  rename(site2=site)

spdiff_climate<-spdiff%>%
  left_join(climate)%>%
  left_join(climate2)%>%
  mutate(arid_diff=abs(datAI-datAI2))

###look for significance

lmresults<-spdiff_climate%>%
  group_by(block_trt)%>%
  summarize(pval=round(summary(lm(species_diff~arid_diff))$coef["arid_diff","Pr(>|t|)"], digits=3),  slope = summary(lm(species_diff~arid_diff))$coef["arid_diff", c("Estimate")])%>%
  mutate(pval2=ifelse(pval==0, "<0.001", as.numeric(round(pval, digits=3))))

ggplot(data=spdiff_climate, aes(x = arid_diff, y = species_diff))+
  geom_point()+
  geom_smooth(data=subset(spdiff_climate, block_trt=="India"), method="lm", color="black", se=F)+
  geom_smooth(data=subset(spdiff_climate, block_trt=="Tibet_ungrazed"), method="lm", color="black", se=F)+
  geom_smooth(data=subset(spdiff_climate, block_trt=="SAmerica_ungrazed"), method="lm", color="black", se=F)+
  geom_smooth(data=subset(spdiff_climate, block_trt=="Brazil"), method="lm", color="black", se=F)+
  facet_wrap(~block_trt, scales="free")+
  theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank())

