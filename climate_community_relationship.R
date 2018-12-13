setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')

library(tidyverse)


#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_climate_Dec2018.csv')%>%
  mutate(ratio=richness/Evar)



#vs aridity
ggplot(data=commClimateSite, aes(x=datAI, y=richness)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free_y') +
  xlab('Aridity') + ylab('Richness')

ggplot(data=commClimateSite, aes(x=datAI, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt) +
  xlab('Aridity') + ylab('Evar')

ggplot(data=commClimateSite, aes(x=datAI, y=ratio)) +
  geom_point() +
  facet_wrap(~block_trt) +
  xlab('Aridity') + ylab('Richness/Evar')

#vs MAP
ggplot(data=commClimateSite, aes(x=bio12, y=richness)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('MAP') + ylab('Richness')

ggplot(data=commClimateSite, aes(x=bio12, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('MAP') + ylab('Evar')

ggplot(data=commClimateSite, aes(x=bio12, y=ratio)) +
  geom_point() +
  facet_wrap(~block_trt) +
  xlab('MAP') + ylab('Richness/Evar')

#rich vs Evar
ggplot(data=commClimateSite, aes(x=richness, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('richness') + ylab('Evar')
