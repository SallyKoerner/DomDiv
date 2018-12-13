setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')

library(tidyverse)


#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_climate_Dec2018.csv')%>%
  mutate(ratio=richness/Evar)



#vs aridity
ggplot(data=commSite, aes(x=datAI, y=richness)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free_y') +
  xlab('Aridity') + ylab('Richness')

ggplot(data=commSite, aes(x=datAI, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt) +
  xlab('Aridity') + ylab('Evar')

ggplot(data=commSite, aes(x=datAI, y=ratio)) +
  geom_point() +
  facet_wrap(~block_trt) +
  xlab('Aridity') + ylab('Richness/Evar')


#rich vs Evar
ggplot(data=commSite, aes(x=richness, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('richness') + ylab('Evar')


#models for each variable
richnessModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(richness ~ datAI, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2])
  
evarModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(Evar ~ datAI, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2])
  
ratioModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(ratio ~ datAI, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2])

  
