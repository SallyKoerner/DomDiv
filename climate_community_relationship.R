setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')

library(grid)
library(tidyverse)


#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_climate_Dec2018.csv')%>%
  mutate(ratio=richness/Evar)


# 
# #vs aridity
# ggplot(data=commSite, aes(x=datAI, y=richness)) +
#   geom_point() +
#   facet_wrap(~block_trt, scales='free_y') +
#   xlab('Aridity') + ylab('Richness') +
#   geom_smooth(data = subset(commSite, block_trt=="India"|block_trt=='NAmerica'|block_trt=='SAfrica'|block_trt=='Tibet_ungrazed'), method='lm', color='black')
# 
# ggplot(data=commSite, aes(x=datAI, y=Evar)) +
#   geom_point() +
#   facet_wrap(~block_trt) +
#   xlab('Aridity') + ylab('Evar') +
#   geom_smooth(data = subset(commSite, block_trt=="India"|block_trt=='Tibet_ungrazed'), method='lm', color='black')
# 
# ggplot(data=commSite, aes(x=datAI, y=ratio)) +
#   geom_point() +
#   facet_wrap(~block_trt) +
#   xlab('Aridity') + ylab('Richness/Evar') +
#   geom_smooth(data = subset(commSite, block_trt=="China"|block_trt=='India'|block_trt=='SAfrica'|block_trt=='NAmerica'|block_trt=='Kenya'|block_trt=='Tibet_ungrazed'), method='lm', color='black')
# 
# 
# #rich vs Evar
# ggplot(data=commSite, aes(x=richness, y=Evar)) +
#   geom_point() +
#   facet_wrap(~block_trt, scales='free') +
#   xlab('richness') + ylab('Evar')



#figure out climate mean, midpoint, min, max
climate <- commSite%>%
  group_by(block_trt)%>%
  summarise(min_AI=min(datAI), max_AI=max(datAI), mean_AI=mean(datAI))%>%
  ungroup()%>%
  mutate(midpoint_AI=(min_AI+max_AI)/2)



#models for each variable
richnessModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(richness ~ datAI, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2])%>%
  left_join(climate)%>%
  mutate(slope_sig=ifelse(pval>0.05, 0, slope))

evarModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(Evar ~ datAI, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2])%>%
  left_join(climate)

ratioModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(ratio ~ datAI, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2])%>%
  left_join(climate)


#model slopes vs aridity (comparing across blocks)
#richness
richnessAllFig <- ggplot(data=commSite, aes(x=datAI, y=richness, color=block_trt)) +
  geom_point() +
  xlab('Aridity') + ylab('Richness') +
  geom_smooth(method='lm') +
  theme(legend.position='none')

summary(lm(slope~midpoint_AI, data=richnessModels))

richnessSlopeFig <- ggplot(data=richnessModels, aes(x=midpoint_AI, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('Aridity') + ylab('Slope of Richness v Aridity') +
  geom_hline(yintercept=0) +
  geom_smooth(method='lm', size=2, color='black') +
  annotate("text", x=1.4, y=125, label = "R2=0.297,\np=0.094", size=8)

#richness figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(richnessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(richnessSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))



#Evar
evennessAllFig <- ggplot(data=commSite, aes(x=datAI, y=Evar, color=block_trt)) +
  geom_point() +
  xlab('Aridity') + ylab('Evar') +
  geom_smooth(method='lm') +
  theme(legend.position='none')

summary(lm(slope~midpoint_AI, data=evarModels))

evennessSlopeFig <- ggplot(data=evarModels, aes(x=midpoint_AI, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('Aridity') + ylab('Slope of Evar v Aridity') +
  geom_hline(yintercept=0) +
  # geom_smooth(method='lm', size=2, color='black') +
  annotate("text", x=1.4, y=-0.6, label = "R2=0.072,\np=0.520", size=8)

#Evar figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(evennessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(evennessSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))



#Ratio richness/Evar
ratioAllFig <- ggplot(data=commSite, aes(x=datAI, y=ratio, color=block_trt)) +
  geom_point() +
  xlab('Aridity') + ylab('Richness/Evar') +
  geom_smooth(method='lm') +
  theme(legend.position='none')

summary(lm(slope~midpoint_AI, data=ratioModels))

ratioSlopeFig <- ggplot(data=ratioModels, aes(x=midpoint_AI, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('Aridity') + ylab('Slope of Richness/Evar v Aridity') +
  geom_hline(yintercept=0) +
  geom_smooth(method='lm', size=2, color='black') +
  annotate("text", x=1.4, y=400, label = "R2=0.526,\np=0.042", size=8)

#ratio figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(ratioAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(ratioSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))