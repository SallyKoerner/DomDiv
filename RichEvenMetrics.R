library(tidyverse)
library(codyn)
library(gridExtra)

setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/")

theme_set(theme_bw(12))

dat<-read.csv("AllData_11Dec2018.csv")%>%
  select(-X)%>%
  filter(plot!="")

#import key to merge community metrics and climate data
key <- read.csv('ClimateSoilsData/SiteNameKey.csv')

#import climate data
climate <- read.csv('ClimateSoilsData/GPScorrClimate.csv')%>%
  select(-X)%>%
  left_join(key)%>%
  filter(!is.na(site))

###getting correct plots to use
plots <- dat%>%
  select(block, site, plot, trt)%>%
  unique()%>%
  left_join(climate)%>%
  mutate(plot_size=quadrat.width*quadrat.length)%>%
  #create block_trt column to account for grazing and plot size differences for a few sites
  mutate(block_trt=as.factor(ifelse(block=='Tibet'&trt=='G', 'Tibet_grazed', ifelse(block=='Tibet'&trt=='U', 'Tibet_ungrazed', ifelse(block=='SAmerica'&trt=='G', 'SAmerica_grazed', ifelse(block=='SAmerica'&trt=='U', 'SAmerica_ungrazed', ifelse(block=='Canada'&trt=='in', 'Canada_ungrazed', ifelse(block=='Canada'&trt=='out', 'Canada_grazed', ifelse(block=='AUS'&plot_size==100, 'AUS_100_m2', ifelse(block=='AUS'&plot_size==900, 'AUS_900_m2', ifelse(block=='AUS'&plot_size==2500, 'AUS_2500_m2', as.character(block))))))))))))%>%
  filter(block!="Canada"&block!='Tanzania'&block!='AUS'&block_trt!='SAmerica_grazed'&block_trt!='Tibet_grazed') #drops datasets we no longer want to include

china <- plots%>%
  filter(block_trt=='China')%>%
  filter(site!='GCN-Dangxiong'&site!="GCN-Naqu"&site!="GCN-Hongyuan"&site!="GCN-Bange")%>% #this site has only 12 reps and is similar in aridity to another site
  mutate(random_num=sample(1:1000, 300, replace=F))%>%
  group_by(site)%>%
  mutate(rank=order(order(random_num)))%>% #create rank order of the random numbers
  ungroup()%>%
  filter(rank<40)%>% #filter to only keep 39 plots per site
  select(-random_num, -rank)

brazil <- plots%>%
  filter(block_trt=='Brazil')%>%
  separate(Site_num, into=c('region', 'CCA', 'rep'), sep='_')%>%
  filter(rep=='PT01')%>% #subsetting out one group from each region to make a site so that there are not tons of sites per region (13 total sites now matching with 13 total region)
  rename(Site_num=rep)%>%
  select(-region, -CCA)

plotssubset <- plots%>%
  filter(block_trt!='China'&block_trt!='Brazil')%>%
  rbind(china)%>%
  rbind(brazil)

#write.csv(plotssubset, "ClimateSoilsData/subset_plots_touse.csv", row.names = F)

##merge these plots in with the community data
data_subset<-dat%>%
  right_join(plotssubset)

write.csv(data_subset, "subet_raw_data.csv", row.names = F)

#calc relative cover
totcov<-data_subset%>%
  group_by(block_trt, site, plot, trt)%>%
  summarise(totcov=sum(cover))

relcov<-dat%>%
  left_join(totcov)%>%
  filter(totcov!=0)%>%
  mutate(relcov=(cover/totcov)*100)%>%
  mutate(unid=paste(block_trt, site, plot, trt, sep="::"))

##doing with an average for each plot.
spave<-relcov%>%
  group_by(block_trt, site, species)%>%
  summarize(ave_relcov=mean(relcov))%>%
  mutate(unid=paste(block_trt, site, sep="::"))

export<-spave%>%
  left_join(key)%>%
  left_join(climate)%>%
  select(-unid)

#write.csv(export, "single_racs_climate_Dec2018.csv", row.names = F)

rich_evar_single<-community_structure(spave, abundance.var = "ave_relcov", replicate.var = "unid", metric = "Evar")%>%
  separate(unid, into=c("block_trt", "site"), sep="::")%>%
  left_join(key)%>%
  left_join(climate)%>%
  filter(richness>4)

SimpD<-function(x, S=length(x[x!=0]), N=sum(x[x!=0]), ps=x[x!=0]/N, p2=ps*ps ){
  D<-sum(p2)
  D
}

Dom<-spave%>%
  group_by(block_trt, site)%>%
  summarise(SimpD=SimpD(ave_relcov),
            BP_D=max(ave_relcov))%>%
  filter(richness>4)

###see how dominance and eveness are related

domeven<-rich_evar_single%>%
  left_join(Dom)

write.csv(domeven, "community_metrics_single_climate_Dec2018.csv", row.names=F)

###dominace is different than Evar. We will try doing these tests with these.
with(domeven, cor.test(Evar, SimpD))
with(domeven, cor.test(Evar, BP_D))

with(domeven, plot(Evar, SimpD))
with(domeven, plot(Evar, BP_D))

pairs(domeven[,c(3,4,40,41)])

#see how number of plots change these relationships
numplots<-data.frame(block_trt=c("Brazil","China", "India","Kenya","NAmerica","SAfrica", "SAmerica_ungrazed", "Tibet_ungrazed"), numplots=c(10,39,9,1,20,20,3,5))

plotnum<-rich_evar_single%>%
  right_join(numplots)%>%
  group_by(block_trt)%>%
  mutate(srich=scale(richness),
         sevar = scale(Evar))


summary(lm(richness~numplots, data=plotnum))
r<-ggplot(data=plotnum, aes(x=numplots, y = richness))+
  geom_point(aes(color=block_trt))+
  geom_smooth(method="lm", color="black", se=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

summary(lm(Evar~numplots, data=plotnum))
e<-ggplot(data=plotnum, aes(x=numplots, y = Evar))+
  geom_point(aes(color=block_trt))+
  geom_smooth(method="lm", color="black", se=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

grid.arrange(r,e, ncol=2)


summary(lm(srich~numplots, data=plotnum))
sr<-ggplot(data=plotnum, aes(x=numplots, y = srich))+
  geom_point(aes(color=block_trt))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

summary(lm(sevar~numplots, data=plotnum))
se<-ggplot(data=plotnum, aes(x=numplots, y = sevar))+
  geom_point(aes(color=block_trt))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


grid.arrange(sr,se, ncol=2)

grid.arrange(r,e, ncol=2)


pairs(rich_evar_single[,3:4])




###we have decided to not do it this way. We are not going to average the richness and evenness of each plot
# rich_evar_plot<-community_structure(relcov, abundance.var = "relcov", replicate.var = "unid", metric = "Evar")%>%
#   separate(unid, into=c("block_trt", "site", "plot", "trt"), sep="::")
# 
# pairs(rich_evar_plot[,5:6])
# 
# write.csv(rich_evar_plot, "community_metrics_plot_Dec2018.csv", row.names=F)

#investigating the relationship richness and evennenss
cor<-rich_evar%>%
  separate(unid, into=c("block", "site", "plot", "trt"), sep="::")%>%
  group_by(block)%>%
  na.omit%>%
  summarize(r.val=cor.test(richness, Evar)$estimate)



