library(tidyverse)
library(codyn)
library(gridExtra)

setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/")
setwd("C:/Users/megha/Dropbox/DomDiv_Workshop/Dominance_Diversity/")


theme_set(theme_bw(12))

rac2 <- read.csv('single_racs_climate_Oct2019.csv')

bt<-unique(rac2$country)

spdiff<-data.frame()

#first do species Differnece looping through the blocks
for (i in 1:length(bt)){
  
  subset<-rac2%>%
    filter(country==bt[i])
  
  sp<-RAC_difference(subset, species.var = "species", abundance.var = "ave_relcov", replicate.var = "site")
  
  sp2<-sp%>%
    mutate(country=bt[i])
  
  spdiff<-rbind(spdiff, sp2)
}

##add climate data
climate<-rac2%>%
  select(country, site, dat.AI)%>%
  unique()

climate2<-rac2%>%
  select(country, site, dat.AI)%>%
  unique()%>%
  rename(datAI2=dat.AI)%>%
  rename(site2=site)

spdiff_climate<-spdiff%>%
  left_join(climate)%>%
  left_join(climate2)%>%
  mutate(arid_diff=abs(dat.AI-datAI2))

###look for significance

lmresults<-spdiff_climate%>%
  group_by(country)%>%
  summarize(pval=round(summary(lm(species_diff~arid_diff))$coef["arid_diff","Pr(>|t|)"], digits=3),  slope = summary(lm(species_diff~arid_diff))$coef["arid_diff", c("Estimate")])%>%
  mutate(pval2=ifelse(pval==0, "<0.001", as.numeric(round(pval, digits=3))))

ggplot(data=spdiff_climate, aes(x = arid_diff, y = species_diff))+
  geom_point()+
  geom_smooth(data=subset(spdiff_climate, country=="India"), method="lm", color="red", se=F)+
  geom_smooth(data=subset(spdiff_climate, country=="Tibet"), method="lm", color="red", se=F)+
  geom_smooth(data=subset(spdiff_climate, country=="Australia"), method="lm", color="red", se=F)+
  geom_smooth(data=subset(spdiff_climate, country=="Brazil"), method="lm", color="red", se=F)+
  geom_smooth(data=subset(spdiff_climate, country=="Kenya"), method="lm", color="red", se=F)+
  geom_smooth(data=subset(spdiff_climate, country=="South Africa"), method="lm", color="red", se=F)+
  geom_smooth(data=subset(spdiff_climate, country=="USA"), method="lm", color="red", se=F)+
  facet_wrap(~country, scales="free")+
  theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank())+
  ylab("Species Difference")+xlab("Aridity Index Difference")

