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
  mutate(plot_size=quadrat.width*quadrat.length)%>%
  #create block_trt column to account for grazing and plot size differences for a few sites
  mutate(block_trt=as.factor(ifelse(block=='Tibet'&trt=='G', 'Tibet_grazed', ifelse(block=='Tibet'&trt=='U', 'Tibet_ungrazed', ifelse(block=='SAmerica'&trt=='G', 'SAmerica_grazed', ifelse(block=='SAmerica'&trt=='U', 'SAmerica_ungrazed', ifelse(block=='Canada'&trt=='in', 'Canada_ungrazed', ifelse(block=='Canada'&trt=='out', 'Canada_grazed', ifelse(block=='AUS'&plot_size==100, 'AUS_100_m2', ifelse(block=='AUS'&plot_size==900, 'AUS_900_m2', ifelse(block=='AUS'&plot_size==2500, 'AUS_2500_m2', as.character(block))))))))))))%>%
  filter(block_trt!='AUS') #drops Australian plots that are mis-sized (very uncommon compared to standard sizes)

china <- commClimate%>%
  filter(block_trt=='China')%>%
  filter(site!='GCN-Dangxiong')%>% #this site has only 12 reps and is similar in aridity to another site
  mutate(random_num=sample(1:1000, 531, replace=F))%>%
  group_by(site)%>%
  mutate(rank=order(order(random_num)))%>% #create rank order of the random numbers
  ungroup()%>%
  filter(rank<40)%>% #filter to only keep 39 plots per site
  select(-random_num, -rank)

brazil <- commClimate%>%
  filter(block_trt=='Brazil')%>%
  separate(Site_num, into=c('region', 'CCA', 'rep'), sep='_')%>%
  filter(rep=='PT01')%>% #subsetting out one group from each region to make a site so that there are not tons of sites per region (13 total sites now matching with 13 total region)
  rename(Site_num=rep)%>%
  select(-region, -CCA)
  
commClimateSubset <- commClimate%>%
  filter(block_trt!='China'&block_trt!='Brazil')%>%
  rbind(china)%>%
  rbind(brazil)%>%
  filter(block!='Canada')

# #plot richness, Evar, EQ vs MAP
# ggplot(data=commClimate, aes(x=bio12, y=richness)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')
# 
# ggplot(data=commClimate, aes(x=bio12, y=Evar)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')
# 
# ggplot(data=commClimate, aes(x=bio12, y=EQ)) +
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
# 
# ggplot(data=commClimate, aes(x=bio1/10, y=EQ)) +
#   geom_point() +
#   facet_wrap(~block, scales='free')


#vs aridity
ggplot(data=commClimate, aes(x=datAI, y=richness)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free_y') +
  xlab('Aridity') + ylab('Richness')

ggplot(data=commClimate, aes(x=datAI, y=Evar)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('Aridity') + ylab('Evar')

ggplot(data=commClimate, aes(x=datAI, y=EQ)) +
  geom_point() +
  facet_wrap(~block_trt, scales='free') +
  xlab('Aridity') + ylab('EQ')

