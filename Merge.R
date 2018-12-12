setwd("~/Dropbox/Dominance_Diversity")

library(ggplot2)
library(MASS)
library(BMA)
library(gdata)
library(leaps)
library(lmSupport)
library(gtools)
library (gridExtra)
library (vegan)
library (reshape2)
library(gtable)
library(relaimpo)
library(doBy)
library(plyr)
library(lme4)
require(GGally)
require(compiler)
require(parallel)
require(boot)
library(metafor)
library(reldist)
library(measurements)
library(tidyverse)

###Import AUS, spread & gather, so all zeros across all sites, get so all have same columns, change cover classes to midpoints
AUS<-read.csv("./Australia/TREND_grassland_species_data_long.csv")%>%
  rename(cover=coverrank)%>%
  mutate(site=paste("site", plot, sep="_"))%>%
  mutate(block="AUS")%>%
  mutate(plot=1)%>%
  mutate(trt=0)%>%
  group_by(block, site, plot, trt, species)%>%
  summarise(cover=mean(cover))%>%
  group_by(block, site, plot, trt, )%>%
  spread(species, cover, fill=0, convert=TRUE)
AUS2<-AUS%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(AUS))%>%
  mutate(cover =as.character(cover))%>%
  mutate(cover2=revalue(cover, c("6"="62.5", "5"= "37.5", "4"="15", 
                                 "3"="3", "2"="3", "1"="1", 
                                 "1.5"="2", "2.5"="3", "3.5"="9")))%>%
  select(-cover)%>%
  rename(cover=cover2)%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(AUS2)

###Import Canada, spread & gather, so all zeros across all sites, get so all have same columns
Canada<-read.csv("./Canada/Canada_Species.csv")%>%
  mutate(block="Canada")%>%
  rename(plot=plot.number)%>%
  rename(trt=exclosure)%>%
  select(block, site, trt, plot, everything())
Canada2<-Canada%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(Canada))%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(Canada2)

###Import all 10 China, spread & gather, so all zeros across all sites, get so all have same columns
### one site in spreadsheet contains no data (Youyu) so it has been left out for now

ChinaA<-read.csv("./China/GCN_Bange.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaA2<-ChinaA%>%
     group_by(block, site, trt, plot,)%>%
    gather(species, cover, 5:ncol(ChinaA))

#This site has very few plots including lack of all but one control
ChinaB<-read.csv("./China/GCN_Dangxiong.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaB2<-ChinaB%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaB))

ChinaC<-read.csv("./China/GCN_Haibei.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaC2<-ChinaC%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaC))

ChinaD<-read.csv("./China/GCN_Hongyuan.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaD2<-ChinaD%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaD))

ChinaE<-read.csv("./China/GCN_Hulunber.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaE2<-ChinaE%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaE))

ChinaF<-read.csv("./China/GCN_Naqu.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaF2<-ChinaF%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaF))


ChinaG<-read.csv("./China/GCN_Yanchi.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaG2<-ChinaG%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaG))

ChinaH<-read.csv("./China/GNC_Urat.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaH2<-ChinaH%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaH))

ChinaI<-read.csv("./China/GNC_Xilinhot.csv")%>%
  select(-cover..., -date, -habitat, -plot)%>%
  rename(trt=Treatment)%>%
  rename(plot=block)%>%
  mutate(block="China")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
ChinaI2<-ChinaI%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(ChinaI))

China_All<-bind_rows(ChinaA2, ChinaB2, ChinaC2, ChinaD2, ChinaE2, ChinaF2, 
                     ChinaG2, ChinaH2, ChinaI2)%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(China_All)
warnings()

###Import India, spread & gather, so all zeros across all sites, get so all have same columns
India<-read.csv("./India/IndiaData.csv")%>%
  replace(is.na(.), 0)%>%
  select(-Type)%>%
  rename(species=Species)
India2<-India%>%
  group_by(species)%>%
  gather(site, cover, 2:ncol(India))%>%
  mutate(block="India")%>%
  mutate(plot=1)%>%
  mutate(trt=0)%>%
  select(block, site, trt, plot, everything())%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(India2)

###Import Kenya, spread & gather, so all zeros across all sites, get so all have same columns
Kenya<-read.csv("./Kenya/KenyaData.csv")%>%
  replace(is.na(.), 0)%>%
  rename(site=SITE)%>%
  mutate(block="Kenya")%>%
  mutate(plot=1)%>%
  mutate(trt=0)%>%
  select(block, site, trt, plot, everything())
Kenya2<-Kenya%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(Kenya))%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(Kenya2)

###Import NA, spread & gather, so all zeros across all sites, get so all have same columns
setwd("~/Dropbox/Dominance_Diversity/North_America/")
na.files <- list.files(pattern=".csv")

for(i in na.files){
  data <- read.csv(i)%>%
  mutate(block="NAmerica")%>%
  mutate(trt=0)%>%
  mutate(plot=paste(Transect, Plot, sep="_"))%>%
  select(-Transect, -Plot, -Plot_ID)%>%
  rename(site=Site)%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())
  
  data2<-data%>%
  group_by(block, site, trt, plot,)%>%
  gather(species, cover, 5:ncol(data))%>%
    ungroup()%>%
    mutate(block=as.character(block), site=as.character(site), 
           plot=as.character(plot), trt=as.character(trt), 
           species=as.character(species), cover=as.numeric(cover))
  
  ifelse(i == na.files[1],NorthAmerica <- data2, NorthAmerica <- bind_rows(NorthAmerica,data2))
}
head(NorthAmerica) 
###Import SAfrica, spread & gather, so all zeros across all sites, get so all have same columns
setwd("~/Dropbox/Dominance_Diversity/South_Africa")
na.files <- list.files(pattern=".csv")

for(i in na.files){
  data <- read.csv(i)%>%
    mutate(block="SAfrica")%>%
    mutate(trt=0)%>%
    mutate(plot=paste(Transect, Plot, sep="_"))%>%
    select(-Transect, -Plot, -Plot_ID)%>%
    rename(site=Site)%>%
    replace(is.na(.), 0)%>%
    select(block, site, trt, plot, everything())
  
  data2<-data%>%
    group_by(block, site, trt, plot,)%>%
    gather(species, cover, 5:ncol(data))%>%
    ungroup()%>%
    mutate(block=as.character(block), site=as.character(site), 
           plot=as.character(plot), trt=as.character(trt), 
           species=as.character(species), cover=as.numeric(cover))  
  
  ifelse(i == na.files[1],SouthAfrica <- data2, SouthAfrica <- bind_rows(SouthAfrica,data2))
}
head(SouthAfrica) 

###Import Canada, spread & gather, so all zeros across all sites, get so all have same columns
setwd("~/Dropbox/Dominance_Diversity")
SAmerica<-read.csv("./South_America/Cesa_Argentina_v2.csv")%>%
  select(-plot, -year, -exage)%>%
  rename(plot=block)%>%
  mutate(block="SAmerica")%>%
  select(block, site, trt, plot, everything())
SAmerica2<-SAmerica%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(SAmerica))%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(SAmerica2)

###Import Tanzania, spread & gather, so all zeros across all sites, get so all have same columns
Tanzania<-read.csv("./Tanzania/TanzaniaData.csv")%>%
  mutate(species=paste(GENUS, SPECIES, sep="_"))%>%
  select(-GENUS, -SPECIES, -SPECIESCODE, -FAMILY, -ID)%>%
  rename(site=PLOT)%>%
  rename(plot=SUBPLOT)%>%
  mutate(block="Tanzania")%>%
  rename(cover=COVER)%>%
  mutate(trt=0)%>%
  select(block, site, trt, plot, species, everything())%>%
  group_by(block, site, plot, trt, species)%>%
  summarise(cover=mean(cover))%>%
  group_by(block, site, plot, trt)%>%
  spread(species, cover, fill=0, convert=TRUE)
Tanzania2<-Tanzania%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(Tanzania))%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(Tanzania2)

###Import Tibet, spread & gather, so all zeros across all sites, get so all have same columns
setwd("~/Dropbox/Dominance_Diversity")
Tibet<-read.csv("./Tibet/Tibet(25)_v3.csv")%>%
  select(-block, -year, -exage)%>%
  mutate(block="Tibet")%>%
  select(block, site, trt, plot, everything())
Tibet2<-Tibet%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(Tibet))%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(Tibet2)

###Import Brazil, spread & gather, so all zeros across all sites, get so all have same columns
setwd("~/Dropbox/Dominance_Diversity")
Brazil<-read.csv("./Brazil/BrazilData.csv")%>%
  rename(site=Plot_code)%>%
  rename(plot=Sampling.unit)%>%
  mutate(species=paste(genero, epiteto, sep="_"))%>%
  select(-id, -uap, -genero, -epiteto, -fisionomia, -Grid_code, 
         -parcela, -Sampling.unit_code, -ordem, -familia, 
         -tribo, -autor, -status, -Espécie)%>%
  mutate(block="Brazil")%>%
  rename(cover=Coverage)%>%
  mutate(trt=0)%>%
  select(block, site, trt, plot, species, everything())%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(Brazil)

#BIND all together
AllData<-bind_rows(AUS2, Brazil, Canada2, China_All, India2, Kenya2, NorthAmerica, 
                   SAmerica2, SouthAfrica, Tanzania2, Tibet2)

head(Canada2)

#Brazil metadat
Brazilmeta<-read.csv("./Brazil/Brazil_metadata.csv")
# convert from decimal minutes to decimal degrees
Brazilmeta$lat = conv_unit(Brazilmeta$lat, 'deg_min_sec', 'dec_deg')
Brazilmeta$long = measurements::conv_unit(Brazilmeta$long, 
                                          from = 'deg_dec_min', to = 'dec_deg')