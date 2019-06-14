setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity")

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
AUS_TREND<-read.csv("./Australia/TREND_grassland_species_data_long.csv")%>%
  rename(cover=coverrank)%>%
  mutate(site=paste("site", plot, sep="_"))%>%
  mutate(block="AUS")%>%
  mutate(plot=1)%>%
  mutate(trt=0)%>%
  group_by(block, site, plot, trt, species)%>%
  summarise(cover=mean(cover))%>%
  group_by(block, site, plot, trt, )%>%
  spread(species, cover, fill=0, convert=TRUE)
AUS_TREND_2<-AUS_TREND%>%
  group_by(block, site, trt, plot)%>%
  gather(species, cover, 5:ncol(AUS_TREND))%>%
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
head(AUS_TREND_2)

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
setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/North_America/")
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
setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/South_Africa")
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
setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity")
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

###Bring in China2
setwd("~/Dropbox/DomDiv_Workshop/Dominance_Diversity/China2")
na.files <- list.files(pattern=".csv")

for(i in na.files){
  data <- read.csv(i)%>%
    mutate(trt=0)%>%
    select(-date)%>%
    replace(is.na(.), 0)%>%
    select(block, site, trt, plot, everything())
  
  data2<-data%>%
    mutate(block=as.character(block), site=as.character(site), 
           plot=as.character(plot), trt=as.character(trt), 
           species=as.character(species), cover=as.numeric(cover))
  
  ifelse(i == na.files[1],China2 <- data2, China2 <- bind_rows(China2,data2))
}
China2<-China2%>%
  select(-X)
head(China2) 

######NEW DATA post Dec 2018 working group
###Bring in China3
setwd("~/Dropbox/DomDiv_Workshop/NewData_ToClean_21Dec2018/China3")

China3A<-read.csv("./Erguna.csv")%>%
  select(-month, -day, -year, -block, -totalcover)%>%
  rename(trt=treatment)%>%
  mutate(block="China3")%>%
  mutate(site="Erguna")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())%>%
  filter(trt=="CON")
China3A2<-China3A%>%
  group_by(block, site, trt, plot, subplot)%>%
  gather(species, cover, 6:ncol(China3A)) %>% 
  group_by(block, site, trt, plot, species)%>%
  summarise(cover=mean(cover))

China3B<-read.csv("./SheilaMuRen.csv")%>%
  select(-month, -year, -block, -total_cover)%>%
  rename(trt=treatments)%>%
  mutate(block="China3")%>%
  mutate(site="SheilaMuRen")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())%>%
  filter(trt=="CON")
China3B2<-China3B%>%
  group_by(block, site, trt, plot, subplot)%>%
  gather(species, cover, 6:ncol(China3B)) %>% 
  group_by(block, site, trt, plot, species)%>%
  summarise(cover=mean(cover))

China3C<-read.csv("./Sher_Tara.csv")%>%
  select(-day, -block, -total_cover)%>%
  rename(trt=treatment)%>%
  mutate(block="China3")%>%
  mutate(site="Sher_Tara")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())%>%
  filter(trt=="CON")
China3C2<-China3C%>%
  group_by(block, site, trt, plot, subplot)%>%
  gather(species, cover, 6:ncol(China3C)) %>% 
  group_by(block, site, trt, plot, species)%>%
  summarise(cover=mean(cover))

China3D<-read.csv("./Urat.csv")%>%
  select(-month, -year, -block, -total_cover)%>%
  rename(trt=treatments)%>%
  mutate(block="China3")%>%
  mutate(site="Urat")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())%>%
  filter(trt=="CON")
China3D2<-China3D%>%
  group_by(block, site, trt, plot, subplot)%>%
  gather(species, cover, 6:ncol(China3D)) %>% 
  group_by(block, site, trt, plot, species)%>%
  summarise(cover=mean(cover))

China3E<-read.csv("./Xilinhot_Leymus.csv")%>%
  select(-month, -year, -day, -block, -total_cover)%>%
  rename(trt=treatments)%>%
  mutate(block="China3")%>%
  mutate(site="Xilinhot_Leymus")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())%>%
  filter(trt=="CON")
China3E2<-China3E%>%
  group_by(block, site, trt, plot, subplot)%>%
  gather(species, cover, 6:ncol(China3E)) %>% 
  group_by(block, site, trt, plot, species)%>%
  summarise(cover=mean(cover))

China3F<-read.csv("./Xilinhot_Stipa.csv")%>%
  select(-month, -year, -day, -block, -total_cover)%>%
  rename(trt=treatment)%>%
  mutate(block="China3")%>%
  mutate(site="Xilinhot_Stipa")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, plot, everything())%>%
  filter(trt=="CON")
China3F2<-China3F%>%
  group_by(block, site, trt, plot, subplot)%>%
  gather(species, cover, 6:ncol(China3F)) %>% 
  group_by(block, site, trt, plot, species)%>%
  summarise(cover=mean(cover))

China3_All<-bind_rows(China3A2, China3B2, China3C2, China3D2, China3E2, China3F2)%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))
head(China3_All)
warnings()

###Bring in Argentina data
setwd("~/Dropbox/DomDiv_Workshop/NewData_ToClean_21Dec2018/Argentina")

ArgentinaA<-read.csv("./ARG_CATA_AND.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="CATA_AND")%>%
  mutate(trt="Grazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Light-crust")%>%
  filter(species!="Litter")
ArgentinaA2<-ArgentinaA%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaA)) %>%  
  select(block, site, trt, plot, everything())

ArgentinaB<-read.csv("./ARG_CATA_SAN.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="CATA_SAN")%>%
  mutate(trt="Grazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Light-crust")%>%
  filter(species!="Litter")
ArgentinaB2<-ArgentinaB%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaB)) %>%  
  select(block, site, trt, plot, everything())

ArgentinaC<-read.csv("./ARG_PATRM_CEX.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="PATRM_CEX")%>%
  mutate(trt="Ungrazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Light crust")%>%
  filter(species!="Bare Soil")%>%
  filter(species!="Litter")%>%
  filter(species!="Moss")
ArgentinaC2<-ArgentinaC%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaC)) %>%  
  select(block, site, trt, plot, everything())

ArgentinaD<-read.csv("./ARG_PATRM_OEX.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="PATRM_OEX")%>%
  mutate(trt="Ungrazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Light crust")%>%
  filter(species!="Bare Soil")%>%
  filter(species!="Dark crust")%>%
  filter(species!="Bare soil")%>%
  filter(species!="Litter")%>%
  filter(species!="Moss")
ArgentinaD2<-ArgentinaD%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaD)) %>%  
  select(block, site, trt, plot, everything())

ArgentinaE<-read.csv("./ARG_PATRM_SEX.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="PATRM_SEX")%>%
  mutate(trt="Ungrazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Light crust")%>%
  filter(species!="Dark crust")%>%
  filter(species!="Bare soil")%>%
  filter(species!="Bare Soil")%>%
  filter(species!="Litter")%>%
  filter(species!="Moss")%>%
  filter(species!="Lichen")

ArgentinaE2<-ArgentinaE%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaE)) %>%  
  select(block, site, trt, plot, everything()) 

ArgentinaF<-read.csv("./ARG-PATNE-RC 3.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="PATNE_RC3")%>%
  mutate(trt="Grazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Biological soil crust moss")

ArgentinaF2<-ArgentinaF%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaF)) %>%  
  select(block, site, trt, plot, everything())

ArgentinaG<-read.csv("./ARG-PATNE-VAL 3.csv")%>%
  rename(species=Species)%>%
  mutate(block="Argentina")%>%
  mutate(site="PATNE_Val3")%>%
  mutate(trt="Grazed")%>%
  replace(is.na(.), 0)%>%
  select(block, site, trt, everything())%>%
  filter(species!="Biological soil crust moss")

ArgentinaG2<-ArgentinaG%>%
  group_by(block, site, trt)%>%
  gather(plot, cover, 5:ncol(ArgentinaG)) %>%  
  select(block, site, trt, plot, everything())

#last argentina site is weird - gave to me in word doc
setwd("~/Dropbox/DomDiv_Workshop/NewData_ToClean_21Dec2018/Argentina/LosPozos")
na.files <- list.files(pattern=".csv")

for(i in na.files){
  data <- read.csv(i)%>%
    mutate(trt="Grazed")%>%
    mutate(block="Argentina")%>%
    mutate(site="LosPozos")%>%
    rename(species="Species") %>% 
    replace(is.na(.), 0)%>%
    select(block, site, trt, everything())%>%
    group_by(block, site, trt)
  
  data2<-data%>%
    gather(plot, cover, 5:ncol(data)) %>%  
    select(block, site, trt, plot, everything()) %>% 
    ungroup()
  
  data3<-data2%>%
    mutate(block=as.character(block), site=as.character(site), 
           plot=as.character(plot), trt=as.character(trt), 
           species=as.character(species), cover=as.numeric(cover))
  
  ifelse(i == na.files[1],Argentina <- data3, Argentina <- bind_rows(Argentina,data3))
}

Argentina_All<-bind_rows(ArgentinaA2, ArgentinaB2, ArgentinaC2, ArgentinaD2, 
                         ArgentinaE2, ArgentinaF2, ArgentinaG2, Argentina)%>%
  ungroup()%>%
  mutate(block=as.character(block), site=as.character(site), 
         plot=as.character(plot), trt=as.character(trt), 
         species=as.character(species), cover=as.numeric(cover))

### bring in AUS from Morgan
setwd("~/Dropbox/DomDiv_Workshop/NewData_ToClean_21Dec2018")

AUS_Morgan<-read.csv("./AUS_Morgan.csv")%>%
  mutate(block="AUS_Morgan")%>%
  replace(is.na(.), 0)%>%
  select(block,everything()) %>% 
  mutate(korrack_M05090=as.numeric(korrack_M05090))

AUS_Morgan2<-AUS_Morgan%>%
  ungroup()%>%
  gather(unique, cover, 3:ncol(AUS_Morgan)) %>%  
  separate(unique, into=c("site", "plot"), sep="_")
  

#BIND all together
AllData<-bind_rows(AUS_TREND_2, Brazil, Canada2, China_All, India2, Kenya2, 
                   NorthAmerica, SAmerica2, SouthAfrica, Tanzania2, Tibet2, China2,
                   China3_All, Argentina_All, AUS_Morgan2)
write.csv(AllData, file="~/Dropbox/DomDiv_Workshop/Dominance_Diversity/AllData_14June2019.csv")





#Brazil metadat
Brazilmeta<-read.csv("./Brazil/Brazil_metadata.csv")
# convert from decimal minutes to decimal degrees
Brazilmeta$lat = conv_unit(Brazilmeta$lat, 'deg_min_sec', 'dec_deg')
Brazilmeta$long = measurements::conv_unit(Brazilmeta$long, 
                                          from = 'deg_dec_min', to = 'dec_deg')