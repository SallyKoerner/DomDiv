setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')

library(tidyverse)

#import community metrics - plot level (each plot represented)
commPlot <- read.csv('community_metrics_plot_Dec2018.csv')

#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_Dec2018.csv')

#import key to merge community metrics and climate data
key <- read.csv('ClimateSoilsData\\SiteNameKey.csv')

#import climate data
climate <- read.csv('ClimateSoilsData\\GPScorrClimate.csv')%>%
  select(-X)%>%
  left_join(key)%>%
  filter(!is.na(site))


###plot level
commClimatePlot <- commPlot%>%
  left_join(climate)%>%
  group_by(block_trt, site)%>%
  summarise_at(c('richness', 'Evar', 'bio1', 'bio12', 'datAI'), mean)%>%
  ungroup()


# #plot richness, Evar vs MAP
# ggplot(data=commClimate, aes(x=bio12, y=richness)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')
# 
# ggplot(data=commClimate, aes(x=bio12, y=Evar)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')



# #vs MAT
# ggplot(data=commClimate, aes(x=bio1/10, y=richness)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')
# 
# ggplot(data=commClimate, aes(x=bio1/10, y=Evar)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')



#vs aridity
ggplot(data=commClimatePlot, aes(x=datAI, y=richness)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free_y') +
  xlab('Aridity') + ylab('Richness')

ggplot(data=commClimatePlot, aes(x=datAI, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('Aridity') + ylab('Evar')



###site level
commClimateSite <- commSite%>%
  left_join(climate)

#vs aridity
ggplot(data=commClimateSite, aes(x=datAI, y=richness)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free_y') +
  xlab('Aridity') + ylab('Richness')

ggplot(data=commClimateSite, aes(x=datAI, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('Aridity') + ylab('Evar')

