###############################################################################
## Purpose: script to analyze the NutNet lubb.us species biomass data
## data spans from 2018 to 2023 
## author : Monika Kelley
###############################################################################

## load packages
library(tidyverse)
#library(ggplot2)  ## do not install again queen 
#library(dplyr)
library(lme4)
library(car)
library(emmeans)
library(vegan)
library(multcompView)
library(multcomp)
library(readxl)
library(lubridate)
library(ggpubr)
library(patchwork)



###############################################################################
## Loading and cleaning data
###############################################################################

### Original data, LBK biomass ----
biomass_00_raw <- read.csv("../data/Biomass.csv")



### Cleaning ---- 

## checking for what categories are there
unique(biomass_00_raw$pft)

## remove "dead" categories
biomass_01_plants <- subset(biomass_00_raw, !(pft == 'dead' | pft == 'Dead'))

## making a new column to standardize growth forms 
# code used to check unique life forms
unique(biomass_01_plants$pft)
unique(biomass_01_plants$growth_form)

# grass
biomass_01_plants$growth_form[biomass_01_plants$pf == 'grasses' | 
                                biomass_01_plants$pft == 'grasses ' |
                                biomass_01_plants$pft == 'grass'] <- 'grass'

# forb
biomass_01_plants$growth_form[biomass_01_plants$pf == 'herbs'] <- 'forb'

# Woody species
biomass_01_plants$growth_form[biomass_01_plants$pf == 'woody'] <- 'woody'


## grouping and summarizing code
# group by plot per year
biomass_02_grouped <- group_by(biomass_01_plants, Plot, Year, DOY, growth_form)
head(biomass_02_grouped)

# means for growth_form weights by plot
biomass_03_mean.weight <- summarise(biomass_02_grouped, biomass.weight = mean(weight))
head(biomass_03_mean.weight)



### adding in additional site and plot information ----

## reading in plot information
plot_information <- read.csv('../data/plot_types.csv')[,2:3]

## adding plot information to biomass data
biomass_04_plot.info <- left_join(biomass_03_mean.weight, plot_information)

## adding specific plot type columns
biomass_04_plot.info <- biomass_04_plot.info %>%
  mutate(n = ifelse(trt == "N" | trt == "NP" | trt == "NK" | trt == "NPK" | trt == "NPK+Fence", 1, 0),
         p = ifelse(trt == "P" | trt == "NP" | trt == "PK" | trt == "NPK" | trt == "NPK+Fence", 1, 0),
         k = ifelse(trt == "K" | trt == "NK" | trt == "PK" | trt == "NPK" | trt == "NPK+Fence", 1, 0))

## add in treatment as binary factors
biomass_04_plot.info$nfac <- as.factor(biomass_04_plot.info$n)
biomass_04_plot.info$pfac <- as.factor(biomass_04_plot.info$p)
biomass_04_plot.info$kfac <- as.factor(biomass_04_plot.info$k)

## add in plot and year as binary factors
biomass_04_plot.info$plotfac <- as.factor(biomass_04_plot.info$Plot)
biomass_04_plot.info$yearfac <- as.factor(biomass_04_plot.info$Year)

## add in blocks 
biomass_04_plot.info$block <- 'block2'
biomass_04_plot.info$block[biomass_04_plot.info$Plot <15] <- 'block1'
biomass_04_plot.info$block[biomass_04_plot.info$Plot >28] <- 'block3'

## removing certain plot types (fence, extra control plots)
biomass_05_cleaned <- subset(biomass_04_plot.info, !(trt == 'Fence' | trt == 'NPK+Fence' |trt == 'xControl'))

## aggregate to a single value for biomass per plot per year
biomass_06_cleaned_groupby <- group_by(biomass_05_cleaned, Plot, Year, DOY, trt, n, p, k,
                                       nfac, pfac, kfac, plotfac, yearfac, block)
biomass_06_cleaned <- summarise(biomass_06_cleaned_groupby, biomass.weight_all = sum(biomass.weight,na.rm=T))

###############################################################################
### Model 01 : Biomass, year and treatments 🤟
###############################################################################

## statistical models of diversity across years
# testing the hypotheses about treatment impacts on community diversity, 
# accounting for year-to-year differences


#### Model 01 : Biomass, year and treatments ----

## model 01
mod_biomass_year.trt <- lmer(log(biomass.weight_all) ~ yearfac * nfac * pfac * kfac + 
                               (1|plotfac) + (1|block), data = biomass_06_cleaned) # log transform to fix normality issue

## check the residuals
# Component-Component plus Residual plot (CCPR plot)
plot(mod_biomass_year.trt, which = 1)
plot(resid(mod_biomass_year.trt) ~ fitted(mod_biomass_year.trt))
hist(biomass_06_cleaned$biomass.weight_all)
hist(log(biomass_06_cleaned$biomass.weight_all))

## Anova
biomass_mod_anova <- Anova(mod_biomass_year.trt)
biomass_mod_anova
summary(biomass_mod_anova)

## post-hoc analyses
emmeans(mod_biomass_year.trt, ~yearfac)




###############################################################################
### visualization
###############################################################################





