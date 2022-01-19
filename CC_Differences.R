library(tidyverse)
library(codyn)

setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/")
theme_set(theme_bw(12))

rac <- read.csv('single_racs_climate_Dec2018.csv')

bt<-unique(rac$block_trt)

cdiff<-data.frame()

#first do Curve Differnece looping through the blocks
for (i in 1:length(bt)){
    
  subset<-rac%>%
      filter(block_trt==bt[i])
    
    cd<-curve_difference(subset, species.var = "species", abundance.var = "ave_relcov", replicate.var = "site")
  
  cd2<-cd%>%
    mutate(block_trt=bt[i])
  
  cdiff<-rbind(cdiff, cd2)
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

cdiff_climate<-cdiff%>%
  left_join(climate)%>%
  left_join(climate2)%>%
  mutate(arid_diff=abs(datAI-datAI2))

###look for significance

lmresults<-cdiff_climate%>%
  group_by(block_trt)%>%
  summarize(pval=round(summary(lm(curve_diff~arid_diff))$coef["arid_diff","Pr(>|t|)"], digits=3),  slope = summary(lm(curve_diff~arid_diff))$coef["arid_diff", c("Estimate")])%>%
  mutate(pval2=ifelse(pval==0, "<0.001", as.numeric(round(pval, digits=3))))

ggplot(data=cdiff_climate, aes(x = arid_diff, y = curve_diff))+
  geom_point()+
  geom_smooth(data=subset(cdiff_climate, block_trt=="India"), method="lm", color="black", se=F)+
  geom_smooth(data=subset(cdiff_climate, block_trt=="Tibet_ungrazed"), method="lm", color="black", se=F)+
  geom_smooth(data=subset(cdiff_climate, block_trt=="SAmerica_ungrazed"), method="lm", color="black", se=F)+
  geom_smooth(data=subset(cdiff_climate, block_trt=="SAfrica"), method="lm", color="black", se=F)+
  facet_wrap(~block_trt, scales="free")+
  theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank())

