### Calculate richness evenness slopes and compare across block aridity gradient
###
### Author: Kevin wilcox (kevin.wilcox@uwyo.edu)
### Last updated: December 13, 2018

###
### Set up workspace
###
rm(list=ls())
library(tidyverse)
library(ggthemes)
library(SLOPE)
setwd("C:\\Users\\wilco\\Dropbox\\DomDiv_Workshop\\Dominance_Diversity\\")


###
### read in relevant data
###
rich_evenness_clmt <- read.csv("community_metrics_single_climate_Dec2018.csv")

###
### Calculate correlations
###

norm_metrics <- rich_evenness_clmt %>%
  group_by(block_trt)%>%
  mutate(
    richness_norm = scale(richness),
    Evar_norm = scale(Evar)
  ) %>%
  dplyr::select(block_trt, richness_norm, Evar_norm) %>%
  ungroup()

scaled_slopes <- norm_metrics %>%
  group_by(block_trt) %>%
  summarize(slope_norm=lm(richness_norm ~ Evar_norm)$coeff[[2]],
            slope_norm_se=summary(lm(richness_norm ~ Evar_norm))$coefficients[2,2],
            pval=summary(lm(richness_norm ~ Evar_norm))$coefficients[2,4]
            )
            
###
### Calculating aridity for each block
###

block_aridity <- rich_evenness_clmt %>%
  group_by(block_trt) %>%
  summarize(max_AI=max(datAI),
            min_AI=min(datAI)) %>%
  ungroup() %>%
  mutate(midpoint_AI = (max_AI+min_AI)/2)
  
slopes_and_aridity <- block_slopes %>%
  full_join(block_aridity, by="block_trt")

scaled_slopes_and_aridity <-  scaled_slopes %>%
  full_join(block_aridity, by="block_trt")


###
### Plot richness-Evar slopes of each block
###
ggplot(rich_evenness_clmt, aes(x=Evar, y=richness)) +
  geom_point()+
  geom_smooth(method="lm",se=F) +
  facet_wrap(~block_trt)

ggplot(norm_metrics, aes(x=Evar_norm, y=richness_norm)) +
  geom_point()+
  geom_smooth(method="lm",se=F) +
  facet_wrap(~block_trt)


###
### Plot slopes versus aridity of blocks
###

ggplot(slopes_and_aridity, aes(x=midpoint_AI, y=slope, label=block_trt)) +
  geom_point() +
  geom_text(nudge_y=50) +
  theme_classic()

ggplot(scaled_slopes_and_aridity, aes(x=midpoint_AI, y=slope_norm, label=block_trt,
                                      xmin=min_AI, xmax=max_AI, ymin=slope_norm-slope_norm_se, ymax=slope_norm+slope_norm_se)) +
  geom_smooth(method="lm",se=T) +
  geom_errorbarh(height=0, col="darkgrey") +
  geom_errorbar(width=0, col="darkgrey") +
  geom_point(size=3) +
#  geom_text(nudge_y=0.05) +
  theme_classic() 

### without china
ggplot(filter(scaled_slopes_and_aridity, block_trt != "China"), aes(x=midpoint_AI, y=slope_norm, label=block_trt,
                                      xmin=min_AI, xmax=max_AI, ymin=slope_norm-slope_norm_se, ymax=slope_norm+slope_norm_se)) +
  geom_hline(yintercept=0, lty=2) +
  geom_smooth(method="lm",se=T) +
  geom_errorbarh(height=0, col="darkgrey") +
  geom_errorbar(width=0, col="darkgrey") +
  geom_point(size=3) +
  #  geom_text(nudge_y=0.05) +
  theme_classic()

###
### Run regression models
###
### * note: we need to run these models using the regression that considers both X and Y uncertainty

#with china
anova(lm(slope_norm ~ midpoint_AI, data=scaled_slopes_and_aridity))
summary(lm(slope_norm ~ midpoint_AI, data=scaled_slopes_and_aridity))

# without China
anova(lm(slope_norm ~ midpoint_AI, data=filter(scaled_slopes_and_aridity, block_trt != "China")))
summary(lm(slope_norm ~ midpoint_AI, data=filter(scaled_slopes_and_aridity, block_trt != "China")))

?geom_errorbarh()

### extras
z <- lm(richness~Evar,  subset(rich_evenness_clmt, block_trt=="India"))
str(summary(z))
summary(z)$coefficients[2,2]

with(subset(rich_evenness, block_trt=="India"), plot(Evar, richness))
pairs(rich_evenness[,5:8])
unique(rich_evenness$block_trt)


?cor.test
?slope
install.packages("SLOPE")
?SLOPE
