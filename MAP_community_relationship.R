setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\GEx working groups\\DomDiv_Workshop\\Dominance_Diversity')

setwd('C:\\Users\\mavolio2\\Dropbox\\DomDiv_Workshop\\Dominance_Diversity')

library(grid)
library(knitr)
library(kableExtra)
library(lme4)
library(tidyverse)
library(lmerTest)
library(MuMIn)
library(gridExtra)
library(gtable)
library(grid)

###ggplot theme set
theme_set(theme_bw(12))
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_text(size=20), legend.text=element_text(size=20))




#import community metrics - site level (single RAC for a site)
commSite <- read.csv('community_metrics_single_climate_Oct2019b.csv')%>%
  group_by(block_trt)%>%
  mutate(Evar_scale=scale(Evar), richness_scale=scale(richness), BP_scale=scale(BP_D))%>%
  ungroup()%>%
  rename(ai=dat.AI)%>%
  filter(country!='Kenya')%>%
  rename(oldcountry=country)%>%
  mutate(country=ifelse(oldcountry=="China", "Inner Mongolia, China", ifelse(oldcountry=="Tibet", "Tibet, China", oldcountry)))


#figure out climate mean, midpoint, min, max
#function for geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

climate <- commSite%>%
  group_by(country)%>%
  summarise(min_MAP=min(bio12), max_MAP=max(bio12), mean_MAP=mean(bio12), geo_mean_MAP=gm_mean(bio12))%>%
  ungroup()%>%
  mutate(midpoint_MAP=(min_MAP+max_MAP)/2)



###models for each variable

#richness models
richnessModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(richness_scale ~ bio12, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)%>%
  mutate(slope_sig=ifelse(pval>0.05, 0, slope))

richnessModelTable <- richnessModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(richnessModelTable, 'html')%>%
  cat(., file = "richnessModelTable_MAP.html")


#quadratic model - AIC= 311.978
summary(quadraticRichnessModel <- lmer(richness_scale~poly(bio12,2) + (1|country), commSite))
AIC(quadraticRichnessModel)
r.squaredGLMM(quadraticRichnessModel)

#linear model - AIC=344.2804
summary(linearRichnessModel <- lmer(richness_scale~bio12 + (1|country), commSite))
AIC(linearRichnessModel)


#evenness models
evarModels <- commSite%>%
  group_by(country)%>%
  do(model = lm(Evar_scale ~ bio12, data = .))%>%
  mutate(R2=summary(model)$r.squared, pval=summary(model)$coefficients[2,4], slope=summary(model)$coefficients[2], slope_err=summary(model)$coefficients[2,2], f=summary(model)$fstatistic[1], df_num=summary(model)$fstatistic[2], df_den=summary(model)$fstatistic[3])%>%
  left_join(climate)

evarModelTable <- evarModels%>%
  select(country, f, df_num, df_den, pval, R2, slope)%>%
  rename(Block=country)
kable(evarModelTable, 'html')%>%
  cat(., file = "evarModelTable_MAP.html")


#quadratic model - AIC=321.9094
summary(quadraticEvenessModel <- lmer(Evar_scale~poly(ai,2) + (1|country), commSite))
AIC(quadraticEvenessModel)
r.squaredGLMM(quadraticEvenessModel)

#evenness model - AIC=517.3496
summary(linearEvenessModel <- lmer(Evar_scale~ai + (1|country), commSite))
AIC(linearEvenessModel)
r.squaredGLMM(linearEvenessModel)



###FIGURES!

#model slopes vs MAP (comparing across blocks)
#richness
richnessAllFig <- ggplot(data=subset(commSite, country!='Kenya'), aes(x=bio12, y=richness_scale, color=country)) +
  xlab('') + ylab('Scaled Richness') +
  scale_color_manual(values=c("#E41A1C", "#999999","#4DAF4A","#984EA3", "#FF7F00",  "#A65628" ,"#F781BF","#377EB8"))+
  #geom_smooth(data=subset(commSite, country=='India'|country=='USA'|country=='South Africa'|country=='Tibet'), method='lm', se=F) +
    #geom_smooth(data=subset(commSite, country=='Brazil'|country=='China'|country=='Kenya'|country=='Argentina'|country=="Australia"), method='lm', linetype='dashed', se=F) +
  geom_smooth(data=commSite, method = "lm", formula = y ~ x + I(x^2), color='black', size=2)+
  geom_point(size=3) +
 # theme(legend.position='none') +
  annotate("text", x=10, y=3, label = "(a)", size=4)+
  annotate("text", x=1600, y=3, label = expression(paste("",R^2 ,"= 0.13")), size=4)+
  labs(color="Country")+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#Evar
evennessAllFig <- ggplot(data=commSite, aes(x=bio12, y=Evar_scale, color=country)) +
  geom_point(size=3) +
  scale_color_manual(values=c("#E41A1C", "#999999","#4DAF4A","#984EA3", "#FF7F00",  "#A65628" ,"#F781BF","#377EB8"))+
  xlab('Mean Annual Precipitation') + ylab('Scaled Evenness') +
  #geom_smooth(data=subset(commSite, country=='India'), method='lm', se=F) +
  #geom_smooth(data=subset(commSite, country=='Brazil'|country=='China'|country=='Kenya'|country=='USA'|country=='South Africa'|country=='Argentina'|country=='Tibet'|country=="Australia"), method='lm', linetype='dashed', se=F) +
  #theme(legend.position='none') +
  labs(color="Gradient")+
  annotate("text", x=10, y=4, label = "(b)", size=4)+
  annotate("text", x=1600, y=4, label = expression(paste("",R^2 ,"= 0.05")), size=4)+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

legend=gtable_filter(ggplot_gtable(ggplot_build(evennessAllFig)), "guide-box") 
grid.draw(legend)

fig1<-
  grid.arrange(arrangeGrob(richnessAllFig+theme(legend.position="none"),
                           evennessAllFig+theme(legend.position="none"),
                           #dominanceAllFig+theme(legend.position="none"),
                           ncol=1), legend, 
               widths=unit.c(unit(1, "npc") - legend$width, legend$width),nrow=1)


  
commSite2<-commSite%>%
    mutate(countrygroup=factor(country, levels = c("Argentina", "Inner Mongolia, China","Tibet, China", "Australia", "South Africa", "USA", "India", "Brazil")))
  
RichMAPTable2<-richnessModelTable%>%
    mutate(countrygroup=factor(Block, levels = c("Argentina", "Inner Mongolia, China","Tibet, China", "Australia", "South Africa", "USA", "India", "Brazil")))%>%
    mutate(r2=round(R2, digits=3))

RichMAPFacet <- ggplot(data=commSite2, aes(x=bio12, y=richness_scale, color=countrygroup)) +
    geom_point() +
    xlab('Mean Annual Precipitation') + ylab('Scaled Richness') +
    geom_smooth(data=subset(commSite2, country=='India'|country=='South Africa'|country=='USA'|country=="Tibet"), method='lm', se=F) +
    theme(legend.position='none') +
    facet_wrap(~countrygroup)+
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
    geom_text(data=RichMAPTable2, mapping=aes(x=Inf, y = Inf, label = r2), hjust=1.05, vjust=1.5, color="black", size=3)+
    theme(strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm")))+
    scale_color_manual(values = c("#E41A1C", "#FF7F00", "#F781BF","#999999", "#A65628", "#377EB8",  "#984EA3", "#4DAF4A"))

EvenMAPTable2<-evarModelTable%>%
  mutate(countrygroup=factor(Block, levels = c("Argentina", "Inner Mongolia, China","Tibet, China", "Australia", "South Africa", "USA", "India", "Brazil")))%>%
  mutate(r2=round(R2, digits=3))

EvenMAPFacet <- ggplot(data=commSite2, aes(x=bio12, y=Evar_scale, color=countrygroup)) +
  geom_point() +
  xlab('Mean Annual Precipitation') + ylab('Scaled Evenness') +
  geom_smooth(data=subset(commSite2, country=='India'), method='lm', se=F) +
  theme(legend.position='none') +
  facet_wrap(~countrygroup)+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_text(data=EvenMAPTable2, mapping=aes(x=Inf, y = Inf, label = r2), hjust=1.05, vjust=1.5, color="black", size=3)+
  theme(strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm")))+
  scale_color_manual(values = c("#E41A1C", "#FF7F00", "#F781BF","#999999", "#A65628", "#377EB8",  "#984EA3", "#4DAF4A"))

