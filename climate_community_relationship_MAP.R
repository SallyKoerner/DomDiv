setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')

library(grid)
library(knitr)
library(kableExtra)
library(lme4)
library(tidyverse)


###ggplot theme set
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))




#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_climate_Dec2018.csv')%>%
  group_by(block_trt)%>%
  mutate(Evar_scale=scale(Evar), richness_scale=scale(richness), BP_scale=scale(BP_D))%>%
  ungroup()%>%
  filter(block_trt!='China') #only 4 datapoints



#figure out climate mean, midpoint, min, max
#function for geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

climate <- commSite%>%
  group_by(block_trt)%>%
  summarise(min_MAP=min(bio12), max_MAP=max(bio12), mean_MAP=mean(bio12), geo_mean_MAP=gm_mean(bio12))%>%
  ungroup()%>%
  mutate(midpoint_MAP=(min_MAP+max_MAP)/2)



###models for each variable

#richness models
richnessModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(richness_scale ~ bio12, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)%>%
  mutate(slope_sig=ifelse(pval>0.05, 0, slope))

richnessModelTable <- richnessModels%>%
  select(block_trt, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=block_trt)
kable(richnessModelTable, 'html')%>%
  cat(., file = "richnessModelTableMAP.html")

# #quadratic model - AIC=529.1192
# summary(quadraticRichnessModel <- lmer(richness_scale~poly(bio12,2) + (1|block_trt), commSite))
# AIC(quadraticRichnessModel)
# #linear model - AIC=549.3088
# summary(linearRichnessModel <- lmer(richness_scale~bio12 + (1|block_trt), commSite))
# AIC(linearRichnessModel)


#evenness models
evarModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(Evar_scale ~ bio12, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

evarModelTable <- evarModels%>%
  select(block_trt, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=block_trt)
kable(evarModelTable, 'html')%>%
  cat(., file = "evarModelTableMAP.html")


#dominance models
domModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(BP_scale ~ bio12, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

domModelTable <- domModels%>%
  select(block_trt, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=block_trt)
kable(domModelTable, 'html')%>%
  cat(., file = "domModelTableMAP.html")

#quadratic model - AIC=543.5141
summary(quadraticDominanceModel <- lmer(BP_scale~poly(bio12,2) + (1|block_trt), commSite))
AIC(quadraticDominanceModel)
#linear model - AIC=563.8917
summary(linearDominanceModel <- lmer(BP_scale~bio12 + (1|block_trt), commSite))
AIC(linearDominanceModel)


#compare richness and evenness
compareEvenModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(Evar_scale ~ richness_scale, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

#compare richness and dominance
compareDomModels <- commSite%>%
  group_by(block_trt)%>%
  do(model = lm(BP_scale ~ richness_scale, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)


###FIGURES!

#model slopes vs aridity (comparing across blocks)
#richness
richnessAllFig <- ggplot(data=commSite, aes(x=bio12, y=richness_scale, color=block_trt)) +
  xlab('MAP') + ylab('Scaled Richness') +
  geom_smooth(data=subset(commSite, block_trt=='India'|block_trt=='NAmerica'|block_trt=='SAfrica'|block_trt=='Tibet_ungrazed'), method='lm', se=F) +
  geom_smooth(data=subset(commSite, block_trt=='Brazil'|block_trt=='China2'|block_trt=='Kenya'|block_trt=='SAmerica_ungrazed'), method='lm', linetype='dashed', se=F) +
  # geom_smooth(data=commSite, method = "lm", formula = y ~ x + I(x^2), color='black', size=2) +
  geom_point(size=5) +
  theme(legend.position='none')

summary(lm(slope~geo_mean_MAP, data=richnessModels))

richnessSlopeFig <- ggplot(data=richnessModels, aes(x=geo_mean_MAP, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_MAP, xmax=max_MAP)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('MAP') + ylab('Slope of Richness v MAP') +
  geom_hline(yintercept=0) +
  # geom_smooth(method='lm', size=2, color='black') +
  annotate("text", x=1500, y=0.01, label = "R2=0.394,\np=0.096", size=8)

#richness figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(richnessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(richnessSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800


#Evar
evennessAllFig <- ggplot(data=commSite, aes(x=bio12, y=Evar_scale, color=block_trt)) +
  geom_point(size=5) +
  xlab('MAP') + ylab('Scaled Evar') +
  geom_smooth(data=subset(commSite, block_trt=='India'|block_trt=='Kenya'), method='lm', se=F) +
  geom_smooth(data=subset(commSite, block_trt=='Brazil'|block_trt=='China2'|block_trt=='India'|block_trt=='NAmerica'|block_trt=='SAfrica'|block_trt=='SAmerica_ungrazed'|block_trt=='Tibet_ungrazed'), method='lm', linetype='dashed', se=F) +
  theme(legend.position='none')

summary(lm(slope~geo_mean_MAP, data=evarModels))

evennessSlopeFig <- ggplot(data=evarModels, aes(x=geo_mean_MAP, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_MAP, xmax=max_MAP)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('MAP') + ylab('Slope of Evar v MAP') +
  geom_hline(yintercept=0) +
  # geom_smooth(method='lm', size=2, color='black') +
  annotate("text", x=1500, y=-0.004, label = "R2=120,\np=0.402", size=8)

#Evar figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(evennessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(evennessSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800


#dominance
dominanceAllFig <- ggplot(data=commSite, aes(x=bio12, y=BP_scale, color=block_trt)) +
  xlab('MAP') + ylab('Scaled Dominance') +
  geom_smooth(data=subset(commSite, block_trt=='India'|block_trt=='SAmerica_ungrazed'|block_trt=='Tibet_ungrazed'), method='lm', se=F) +
  geom_smooth(data=subset(commSite, block_trt=='Brazil'|block_trt=='Kenya'|block_trt=='NAmerica'|block_trt=='SAfrica'|block_trt=='China2'), method='lm', linetype='dashed', se=F) +
  geom_point(size=5) +
  theme(legend.position='none')

summary(lm(slope~geo_mean_MAP, data=domModels))

dominanceSlopeFig <- ggplot(data=domModels, aes(x=geo_mean_MAP, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_MAP, xmax=max_MAP)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('MAP') + ylab('Slope of Dominance v MAP') +
  geom_hline(yintercept=0) +
  # geom_smooth(method='lm', size=2, color='black') +
  annotate("text", x=1500, y=-0.004, label = "R2=0.294,\np=0.165", size=8)

#dominance figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(dominanceAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(dominanceSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800



#Evar v richness
compareAllFig <- ggplot(data=commSite, aes(x=richness_scale, y=Evar_scale, color=block_trt)) +
  geom_point() +
  xlab('Scaled Richness') + ylab('Scaled Evar') +
  geom_smooth(data=subset(commSite, block_trt=='India'|block_trt=='Brazil'|block_trt=='NAmerica'), method='lm', se=F) +
  theme(legend.position='none') +
  facet_wrap(~block_trt)

summary(lm(slope~geo_mean_MAP, data=compareEvenModels))

compareSlopeFig <- ggplot(data=compareEvenModels, aes(x=geo_mean_MAP, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_MAP, xmax=max_MAP)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('MAP') + ylab('Slope of Evar v Richness') +
  geom_hline(yintercept=0) +
  annotate("text", x=1500, y=-0.5, label = "R2=0.303,\np=0.158", size=8)

#comparison figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(compareAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(compareSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800



#dominance v richness
compareAllFig <- ggplot(data=commSite, aes(x=richness_scale, y=BP_scale, color=block_trt)) +
  geom_point() +
  xlab('Scaled Richness') + ylab('Scaled Dominance') +
  geom_smooth(data=subset(commSite, block_trt=='India'|block_trt=='Kenya'|block_trt=='Tibet_ungrazed'), method='lm', se=F) +
  theme(legend.position='none') +
  facet_wrap(~block_trt)

summary(lm(slope~geo_mean_MAP, data=compareDomModels))

compareSlopeFig <- ggplot(data=compareDomModels, aes(x=geo_mean_MAP, y=slope, color=block_trt)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin=min_MAP, xmax=max_MAP)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('MAP') + ylab('Slope of Dominance v Richness') +
  geom_hline(yintercept=0) +
  annotate("text", x=1000, y=-0.75, label = "R2=0.051,\np=0.589", size=8)

#comparison figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(compareAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(compareSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800










