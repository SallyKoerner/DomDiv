setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\DomDiv_Workshop\\Dominance_Diversity')
setwd('C:\\Users\\megha\\Dropbox\\DomDiv_Workshop\\Dominance_Diversity')

library(grid)
library(knitr)
library(kableExtra)
library(lme4)
library(tidyverse)
library(lmerTest)

###ggplot theme set
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))




#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_climate_Oct2019.csv')%>%
  group_by(block_trt)%>%
  mutate(Evar_scale=scale(Evar), richness_scale=scale(richness), BP_scale=scale(BP_D))%>%
  ungroup()%>%
  rename(ai=dat.AI)


#figure out climate mean, midpoint, min, max
#function for geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

climate <- commSite%>%
  group_by(country)%>%
  summarise(min_AI=min(ai), max_AI=max(ai), mean_AI=mean(ai), geo_mean_AI=gm_mean(ai))%>%
  ungroup()%>%
  mutate(midpoint_AI=(min_AI+max_AI)/2)



###models for each variable

#richness models
richnessModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(richness_scale ~ ai, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)%>%
  mutate(slope_sig=ifelse(pval>0.05, 0, slope))

richnessModelTable <- richnessModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(richnessModelTable, 'html')%>%
  cat(., file = "richnessModelTable.html")

#quadratic model - AIC=484.82
summary(quadraticRichnessModel <- lmer(richness_scale~poly(ai,2) + (1|country), commSite))
AIC(quadraticRichnessModel)
#linear model - AIC=519.97
summary(linearRichnessModel <- lmer(richness_scale~ai + (1|country), commSite))
AIC(linearRichnessModel)


#evenness models
evarModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(Evar_scale ~ ai, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

evarModelTable <- evarModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(evarModelTable, 'html')%>%
  cat(., file = "evarModelTable.html")

#quadratic model - AIC=512.41
summary(quadraticEvenessModel <- lmer(Evar_scale~poly(ai,2) + (1|country), commSite))
AIC(quadraticEvenessModel)
#quadratic model - AIC=517.41
summary(linearEvenessModel <- lmer(Evar_scale~ai + (1|country), commSite))
AIC(linearEvenessModel)

#compare richness and evenness
compareEvenModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(Evar_scale ~ richness_scale, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

compareEvenModelTable <- compareEvenModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(compareEvenModelTable, 'html')%>%
  cat(., file = "compareEvenModelTable")


#dominance models
domModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(BP_scale ~ ai, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

domModelTable <- domModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(domModelTable, 'html')%>%
  cat(., file = "domModelTable.html")

#quadratic model - AIC=512.54
summary(quadraticDominanceModel <- lmer(BP_scale~poly(ai,2) + (1|country), commSite))
AIC(quadraticDominanceModel)
#linear model - AIC=520.04
summary(linearDominanceModel <- lmer(BP_scale~ai + (1|country), commSite))
AIC(linearDominanceModel)


#compare richness and dominance
compareDomModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(BP_scale ~ richness_scale, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

compareDomModelTable <- compareDomModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(compareDomModelTable, 'html')%>%
  cat(., file = "compareDomModelTable")




###FIGURES!

#model slopes vs aridity (comparing across blocks)
#richness
richnessAllFig <- ggplot(data=commSite, aes(x=ai, y=richness_scale, color=country)) +
  xlab('') + ylab('Scaled Richness') +
  scale_color_brewer(palette="Set1")+
  geom_smooth(data=subset(commSite, country=='India'|country=='USA'|country=='South Africa'|country=='Tibet'), method='lm', se=F) +
  
  geom_smooth(data=subset(commSite, country=='Brazil'|country=='China'|country=='Kenya'|country=='Argentina'|block_trt=="Australia"), method='lm', linetype='dashed', se=F) +
  geom_smooth(data=commSite, method = "lm", formula = y ~ x + I(x^2), color='black', size=2) +
  geom_point(size=5) +
  theme(legend.position='none') +
  annotate("text", x=0.1, y=3, label = "(a)", size=6)

# summary(lm(slope~geo_mean_AI, data=richnessModels))
# 
# richnessSlopeFig <- ggplot(data=richnessModels, aes(x=geo_mean_AI, y=slope, color=block_trt)) +
#   geom_point(size=5) +
#   geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
#   geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
#   xlab('Aridity') + ylab('Slope of Richness v Aridity') +
#   geom_hline(yintercept=0) +
#   geom_smooth(method='lm', size=2, color='black') +
#   annotate("text", x=1.4, y=6, label = "R2=0.530,\np=0.041", size=8)
# 
# #richness figure
# pushViewport(viewport(layout=grid.layout(1,2)))
# print(richnessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
# print(richnessSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
# #export at 1800x800


#Evar
evennessAllFig <- ggplot(data=commSite, aes(x=ai, y=Evar_scale, color=country)) +
  geom_point(size=5) +
  scale_color_brewer(palette="Set1")+
  xlab('') + ylab('Scaled Evar') +
  geom_smooth(data=subset(commSite, country=='India'), method='lm', se=F) +
  geom_smooth(data=subset(commSite, country=='Brazil'|country=='China'|country=='Kenya'|country=='USA'|country=='South Africa'|country=='Argentina'|country=='Tibet'|country=="Australia"), method='lm', linetype='dashed', se=F) +
  #theme(legend.position='none') +
  annotate("text", x=0.1, y=4, label = "(b)", size=6)

# summary(lm(slope~geo_mean_AI, data=evarModels))
# 
# evennessSlopeFig <- ggplot(data=evarModels, aes(x=geo_mean_AI, y=slope, color=block_trt)) +
#   geom_point(size=5) +
#   geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
#   geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
#   xlab('Aridity') + ylab('Slope of Evar v Aridity') +
#   geom_hline(yintercept=0) +
#   # geom_smooth(method='lm', size=2, color='black') +
#   annotate("text", x=1.2, y=4, label = "R2=0.012,\np=0.793", size=8)
# 
# #Evar figure
# pushViewport(viewport(layout=grid.layout(1,2)))
# print(evennessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
# print(evennessSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
# #export at 1800x800


#dominance
dominanceAllFig <- ggplot(data=commSite, aes(x=ai, y=BP_scale, color=country)) +
  xlab('Aridity') + ylab('Scaled Dominance') +
  scale_color_brewer(palette="Set1")+
  geom_smooth(data=subset(commSite, country=='Tibet'), method='lm', se=F) +
  geom_smooth(data=subset(commSite, country=='Brazil'|country=='India'|country=='Kenya'|country=='USA'|country=='South Africa'|country=='China'|country=="Argentina"|country=="Australia"), method='lm', linetype='dashed', se=F) +
  geom_point(size=5) +
  #theme(legend.position='none') + 
  geom_smooth(data=commSite, method = "lm", formula = y ~ x + I(x^2), color='black', size=2) +
  annotate("text", x=0.1, y=4.5, label = "(c)", size=6)

# summary(lm(slope~geo_mean_AI, data=domModels))
# 
# dominanceSlopeFig <- ggplot(data=domModels, aes(x=geo_mean_AI, y=slope, color=block_trt)) +
#   geom_point(size=5) +
#   geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
#   geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
#   xlab('Aridity') + ylab('Slope of Dominance v Aridity') +
#   geom_hline(yintercept=0) +
#   # geom_smooth(method='lm', size=2, color='black') +
#   annotate("text", x=1.2, y=-4, label = "R2=0.403,\np=0.091", size=8)
# 
# #dominance figure
# pushViewport(viewport(layout=grid.layout(1,2)))
# print(dominanceAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
# print(dominanceSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
# #export at 1800x800

#community metrics vs aridity figure
# pushViewport(viewport(layout=grid.layout(3,1)))
# print(richnessAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
# print(evennessAllFig, vp=viewport(layout.pos.row=2, layout.pos.col=1))
# print(dominanceAllFig, vp=viewport(layout.pos.row=3, layout.pos.col=1))
# #export at 600x1800

legend=gtable_filter(ggplot_gtable(ggplot_build(dominanceAllFig)), "guide-box") 
grid.draw(legend)

fig1<-
  grid.arrange(arrangeGrob(richnessAllFig+theme(legend.position="none"),
                           evennessAllFig+theme(legend.position="none"),
                           dominanceAllFig+theme(legend.position="none"),
                           ncol=1), legend, 
               widths=unit.c(unit(1, "npc") - legend$width, legend$width),nrow=1)


#Evar v richness
compareAllFig <- ggplot(data=commSite, aes(x=richness_scale, y=Evar_scale, color=country)) +
  geom_point() +
  scale_color_brewer(palette="Set1")+
  xlab('Scaled Richness') + ylab('Scaled Evar') +
  geom_smooth(data=subset(commSite, country=='India'|country=='Brazil'|country=='USA'|country=="China"), method='lm', se=F) +
  theme(legend.position='none') +
  facet_wrap(~country)

summary(lm(slope~geo_mean_AI, data=compareEvenModels))

compareSlopeFig <- ggplot(data=compareEvenModels, aes(x=geo_mean_AI, y=slope, color=country)) +
  geom_point(size=5) +
  scale_color_brewer(palette="Set1")+
  geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('Aridity') + ylab('Slope of Evar v Richness') +
  geom_hline(yintercept=0) +
  annotate("text", x=1.1, y=-0.8, label = expression(paste("",R^2 ,"= 0.06, p=0.255")), size=5)

#comparison figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(compareAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(compareSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800



#dominance v richness
compareAllFig <- ggplot(data=commSite, aes(x=richness_scale, y=BP_scale, color=country)) +
  geom_point() +
  scale_color_brewer(palette="Set1")+
  xlab('Scaled Richness') + ylab('Scaled Dominance') +
  geom_smooth(data=subset(commSite, country=='India'|country=='Kenya'|country=='Tibet'|country=="Argentina"|country=="Australia"), method='lm', se=F) +
  theme(legend.position='none') +
  facet_wrap(~country)

summary(lm(slope~geo_mean_AI, data=compareDomModels))

compareSlopeFig <- ggplot(data=compareDomModels, aes(x=geo_mean_AI, y=slope, color=country)) +
  geom_point(size=5) +
  scale_color_brewer(palette="Set1")+
  geom_errorbarh(aes(xmin=min_AI, xmax=max_AI)) +
  geom_errorbar(aes(ymin=slope-slope_err, ymax=slope+slope_err)) +
  xlab('Aridity') + ylab('Slope of Dominance v Richness') +
  geom_hline(yintercept=0) +
  annotate("text", x=1.05, y=-0.95, label = expression(paste("",R^2 ,"= 0.13, p=0.182")), size=5)

#comparison figure
pushViewport(viewport(layout=grid.layout(1,2)))
print(compareAllFig, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(compareSlopeFig, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#export at 1800x800










