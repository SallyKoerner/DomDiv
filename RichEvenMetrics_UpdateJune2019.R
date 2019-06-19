library(tidyverse)
library(codyn)
library(gridExtra)

setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/")

theme_set(theme_bw(12))

dat<-read.csv("AllData_14June2019.csv")%>%
  select(-X)%>%
  filter(plot!="")

#import key to merge community metrics and climate data
key <- read.csv('ClimateSoilsData/SiteNameKey_UpdateJune2019.csv')

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
  filter(block!="Canada"&block!='Tanzania'&block!='AUS'&block_trt!='SAmerica_grazed'&block_trt!='Tibet_grazed'&site!="Tibet_Site1_Amdo_Bang'ai"&site!="Tibet_Site6_Shuanghu_Beicuo") #drops datasets we no longer want to including two tibetan sites that are missing 2 of the 5 plots

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

samerica <- plots%>%
  filter(block_trt=='SAmerica_ungrazed')%>%
  filter(plot!='4', plot!="5") #subsetting out two of the plots from only two sites which had 5 transects instead of 3

### need to do AUS_Morgan --- drop all sites with less than 5 plots (check to make sure this still spans a good precip gradient), and then randomly subsample other sites with more than 5 to pick just 5 -- do like China above

#### need to do China3 ---- USE ALL PLOTS, do not need to subset this site

#### need to do Argentina ---- USE ALL PLOTS, do not need to subset this site, there are 100 plots per site though, so if this looks weird, we might need to rethink


plotssubset <- plots%>%
  filter(block_trt!='China'&block_trt!='Brazil'&block_trt!='SAmerica_ungrazed')%>%
  rbind(china)%>%
  rbind(brazil)%>%
  rbind(samerica)

#write.csv(plotssubset, "ClimateSoilsData/subset_plots_touse_14Dec2018.csv", row.names = F)

##merge these plots in with the community data
data_subset<-dat%>%
  right_join(plotssubset)

#write.csv(data_subset, "subet_raw_data.csv", row.names = F)

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


BP<-spave%>%
  group_by(block_trt, site)%>%
  summarise(BP_D=max(ave_relcov))

###see how dominance and eveness are related

domeven<-rich_evar_single%>%
  left_join(BP)

write.csv(domeven, "community_metrics_single_climate_Dec2018.csv", row.names=F)

###dominace is different than Evar. We will try doing these tests with these.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
    test <- cor.test(x,y) 
    text(0.5, 0.5, txt, cex = 2) 
  text(.8, .8, "*", cex=4, col="red") 
}

pairs(domeven[,c(3,4,39)], lower.panel=panel.smooth, upper.panel=panel.cor)


#see how number of plots change these relationships
numplots<-data.frame(block_trt=c("Brazil","China2", "India","Kenya","NAmerica","SAfrica", "SAmerica_ungrazed", "Tibet_ungrazed"), numplots=c(10,6,9,1,20,20,3,5))

plotnum<-domeven%>%
  right_join(numplots)%>%
  group_by(block_trt)%>%
  mutate(srich=scale(richness),
         sevar = scale(Evar),
         sdom = scale(BP_D))


summary(lm(richness~numplots, data=plotnum))
r<-ggplot(data=plotnum, aes(x=numplots, y = richness))+
  geom_point(aes(color=block_trt))+
  scale_color_brewer(palette="Set1")+
  geom_smooth(method="lm", color="black", se=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Number of Plots")+
  ylab("Richness")

summary(lm(Evar~numplots, data=plotnum))
e<-ggplot(data=plotnum, aes(x=numplots, y = Evar))+
  geom_point(aes(color=block_trt))+
  scale_color_brewer(palette="Set1")+
  geom_smooth(method="lm", color="black", se=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Number of Plots")+
  ylab("Evenness")

summary(lm(BP_D~numplots, data=plotnum))
d<-ggplot(data=plotnum, aes(x=numplots, y = Evar))+
  geom_point(aes(color=block_trt))+
  scale_color_brewer(palette="Set1")+
  geom_smooth(method="lm", color="black", se=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Number of Plots")+
  ylab("Dominance")

summary(lm(srich~numplots, data=plotnum))
sr<-ggplot(data=plotnum, aes(x=numplots, y = srich))+
  geom_point(aes(color=block_trt))+
  scale_color_brewer(palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Scaled Richness")+
  xlab("Number of Plots")

summary(lm(sevar~numplots, data=plotnum))
se<-ggplot(data=plotnum, aes(x=numplots, y = sevar))+
  geom_point(aes(color=block_trt))+
  scale_color_brewer(palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Scaled Evenness")+
  xlab("Number of Plots")

summary(lm(sdom~numplots, data=plotnum))
sd<-ggplot(data=plotnum, aes(x=numplots, y = sdom))+
  geom_point(aes(color=block_trt))+
  scale_color_brewer(palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Scaled Dominance")+
  xlab("Number of Plots")

grid.arrange(r,e,d, sr,se,sd, ncol=3)


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



