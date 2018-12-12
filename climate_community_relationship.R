setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')

library(tidyverse)

#import community metrics
comm <- read.csv('community_metrics_12122018.csv')

#import key to merge community metrics and climate data
key <- read.csv('ClimateSoilsData\\SiteNameKey.csv')

#import climate data
climate <- read.csv('ClimateSoilsData\\GPScorrClimate.csv')%>%
  select(-X)%>%
  left_join(key)%>%
  filter(!is.na(site))

commClimate <- comm%>%
  left_join(climate)%>%
  group_by(block, site)%>%
  summarise_at(c('richness', 'Evar', 'EQ', 'bio1', 'bio12', 'datAI'), mean)%>%
  ungroup()


#plot richness, Evar, EQ vs MAP
ggplot(data=commClimate, aes(x=bio12, y=richness)) +
  geom_point() +
  facet_wrap(~block, scales='free')

ggplot(data=commClimate, aes(x=bio12, y=Evar)) +
  geom_point() +
  facet_wrap(~block, scales='free')

ggplot(data=commClimate, aes(x=bio12, y=EQ)) +
  geom_point() +
  facet_wrap(~block, scales='free')


#vs MAT
ggplot(data=commClimate, aes(x=bio1/10, y=richness)) +
  geom_point() +
  facet_wrap(~block, scales='free')

ggplot(data=commClimate, aes(x=bio1/10, y=Evar)) +
  geom_point() +
  facet_wrap(~block, scales='free')

ggplot(data=commClimate, aes(x=bio1/10, y=EQ)) +
  geom_point() +
  facet_wrap(~block, scales='free')


#vs aridity
ggplot(data=commClimate, aes(x=datAI, y=richness)) +
  geom_point() +
  facet_wrap(~block, scales='free')

ggplot(data=commClimate, aes(x=datAI, y=Evar)) +
  geom_point() +
  facet_wrap(~block, scales='free')

ggplot(data=commClimate, aes(x=datAI, y=EQ)) +
  geom_point() +
  facet_wrap(~block, scales='free')

