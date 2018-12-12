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

###Import All Datasets
All<-read.csv("./AllData_11Dec2018.csv")%>%
  select(-X)

###Getting Relative Cover
totcov<-All%>%
  tbl_df()%>%
  group_by(block, site, plot, trt)%>%
  summarise(totcov=sum(cover))

relcov<-merge(totcov, All, by=c("block", "site", "plot", "trt"))%>%
  mutate(relcov=(cover/totcov)*100)

#####At what level do we want to do these analyses or merge the "plots"?

####FUNCS for Evenness
E_q<-function(x){
  x1<-x[x!=0]
  if (length(x1)==1) {
    return(NA)
  }
  if (abs(max(x1) - min(x1)) < .Machine$double.eps^0.5) {##bad idea to test for zero, so this is basically doing the same thing testing for a very small number
    return(1)
  }
  r<-rank(x1, ties.method = "average")
  r_scale<-r/max(r)
  x_log<-log(x1)
  fit<-lm(r_scale~x_log)
  b<-fit$coefficients[[2]]
  2/pi*atan(b)
}


#function to calculate E1/D (inverse of Simpson's) from Smith and Wilson 1996
#' @S the number of species in the sample
#' @x the vector of abundances of each species
#' @N the total abundance
#' @p the vector of relative abundances of each species
E_simp<-function(x, S=length(x[x!=0]), N=sum(x[x!=0]), ps=x[x!=0]/N, p2=ps*ps ){
  D<-sum(p2)
  (1/D)/S
}

SimpD<-function(x, S=length(x[x!=0]), N=sum(x[x!=0]), ps=x[x!=0]/N, p2=ps*ps ){
  D<-sum(p2)
  D
}
#calculating gini coefficeint using the gini function in the reldist package
#' @x the vector of abundances of each species
#' this tive the inverse of other measures of evenness??
Gini<-function(x){
  x1<-x[x!=0]
  1-reldist::gini(x1)
}


metric1<-relcov%>%
  filter(cover!=0)%>%
  group_by(block, site, plot, trt)%>%
  summarise(S=n(), 
            div=diversity(relcov), 
            even=div/log(S),
            E_Q=E_q(relcov),
            ESimp=E_simp(relcov),
            Gini=Gini(relcov),
            SimpD=SimpD(relcov),
            BP_D=max(relcov))
