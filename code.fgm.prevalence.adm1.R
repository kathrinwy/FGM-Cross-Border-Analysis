
# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# Author name: Kathrin Weny


# File description, purpose of code, inputs and output --------------------

# Extracts prevalence and ethnicity estimates from household surveys

# Source codes, libraries, global options and working directory -----------

library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(survey)
library(foreign)
library(reshape2)
library(survminer)
library(data.table)
library(tmap)
library(leaflet)
library(rio)
library(tmaptools)
library(rgdal)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2,sp,raster,maps,mapdata,maptools,ggmap,rgeos)
library(map)
library(raster)
library(tidyverse)
library(broom)
library(maps)
library(ggrepel)
options(scipen = 999)  # disable scientific notation in R

#if a primary sapling unit has only a single observation, R will crash
# option adjust calculates conservative standard errors
# reference: http://faculty.washington.edu/tlumley/survey/example-lonely.html
options(survey.lonely.psu = "adjust")

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Surveys")

listsav.dhs <- dir(pattern = "*.SAV")
listsav.dhs <- listsav.dhs[-c(2,6,7,12)] #remove Egypt, Kenya, Liberia, Uganda from loop -- different database design

# Executed statements -----------------------------------------------------

# DHS - I -----------------------------------------------------------------

# create empty list to store results in loop
results.fgm <- data.frame(matrix(NA, nrow = 0, ncol = 4))

# Loop estimating FGM prevalence at ADM1
for(i in 1:length(listsav.dhs)){
  
  # Read in survey data
  data <- read.spss(listsav.dhs[i], to.data.frame=TRUE) 

  # select variables and create dataset for birth reshape and FGM reshape
  data_short <- data %>%
    dplyr::select(c("V000", "V005","V021","V022","V024", "V131", "G100", "G102"))
  
  data_short$G100 <- as.numeric(data_short$G100)
  data_short$G102 <- as.numeric(data_short$G102)
  
  # recode FGM - if woman has not heard of FGM she is assumed not to have experienced it
  data_short$fgm <- ifelse(data_short$G100 == 1, 1, data_short$G102)
  data_short$fgm <- ifelse(data_short$fgm == 1, 0, 1)
  
  # create a complex sample survey object to calculate standard errors with taylor-series linearization
  survey.data <- svydesign(id             = ~V021, 
                           strata         = ~V022, 
                           variables      = ~fgm + V024 + V131,
                           weight         = data_short$V005,
                           data           = data_short)
  
  # Estimate prevalence of fgm by ADMIN1 (V024)  
  tmp.fgm <- as.data.frame(svyby(~fgm, ~V024, survey.data, svymean, na.rm = TRUE))

  tmp.fgm$country <- data$V000[1]
  
  results.fgm <- rbind(results.fgm, setnames(tmp.fgm, names(results.fgm)))
  
}


# DHS - II ----------------------------------------------------------------
# This section is for surveys that do not follow the standardized format

# EGYPT (2015)
data <- read.spss("Egypt.SAV", to.data.frame=TRUE) 

# select variables 
data_short <- data %>%
  dplyr::select(c("V000", "V005","V021","V022","SGOV", "G100", "G102")) # V131 = NA

data_short$G100 <- as.numeric(data_short$G100)
data_short$G102 <- as.numeric(data_short$G102)

# recode fGM
data_short$fgm <- ifelse(data_short$G100 == 1, 1, data_short$G102)
data_short$fgm <- ifelse(data_short$fgm == 1, 0, 1)

# create a complex sample survey object to calculate standard errors with taylor-series linearization
survey.data <- svydesign(id             = ~V021, 
                         strata         = ~V022, 
                         variables      = ~fgm + SGOV,
                         weight         = data_short$V005,
                         data           = data_short)


tmp <- as.data.frame(svyby(~fgm, ~SGOV, survey.data, svymean, na.rm = TRUE))

tmp$country <- data$V000[1]

results.fgm <- rbind(results.fgm,setnames(tmp,names(results.fgm)))

# KENYA

data <- read.spss("Kenya.SAV", to.data.frame=TRUE) 
a <- as.data.frame(attr(data, "variable.labels"))

# select variables and create dataset for birth reshape and FGM reshape
data_short <- data %>%
  dplyr::select(c("V000", "V005","V021","V022","V131", "SREGION", "G100", "G102"))

data_short$G100 <- as.numeric(data_short$G100)
data_short$G102 <- as.numeric(data_short$G102)

# recode FGM
data_short$fgm <- ifelse(data_short$G100 == 1, 1, data_short$G102)
data_short$fgm <- ifelse(data_short$fgm == 1, 0, 1)

# create a complex sample survey object to calculate standard errors with taylor-series linearization
survey.data <- svydesign(id             = ~V021, 
                         strata         = ~V022, 
                         variables      = ~fgm + SREGION + V131,
                         weight         = data_short$V005,
                         data           = data_short)


tmp <- as.data.frame(svyby(~fgm, ~SREGION, survey.data, svymean, na.rm = TRUE))

tmp$country <- data$V000[1]

results.fgm <- rbind(results.fgm, setnames(tmp, names(results.fgm)))

# LIBERIA
data <- read.spss("Liberia.SAV", to.data.frame=TRUE) 

# select variables and create dataset for birth reshape and FGM reshape
data_short <- data %>%
  dplyr::select(c("V000", "V005","V021","V022","SCOUNTY", "S951", "S952")) #V131 is dialect for Liberia

data_short$S951 <- as.numeric(data_short$S951)
data_short$S952 <- as.numeric(data_short$S952)

# recode fGM
data_short$fgm <- ifelse(data_short$S951 == 1, 1, data_short$S952)
data_short$fgm <- ifelse(data_short$fgm == 1, 0, 1)


# create a complex sample survey object to calculate standard errors with taylor-series linearization
survey.data <- svydesign(id             = ~V021, 
                         strata         = ~V022, 
                         variables      = ~fgm + SCOUNTY,
                         weight         = data_short$V005,
                         data           = data_short)


tmp <- as.data.frame(svyby(~fgm, ~SCOUNTY, survey.data, svymean, na.rm = TRUE))

tmp$country <- data$V000[1]

results.fgm <- rbind(results.fgm,setnames(tmp,names(results.fgm)))

# UGANDA
data <- read.spss("Uganda.SAV", to.data.frame=TRUE) 

# select variables and create dataset for birth reshape and FGM reshape
data_short <- data %>%
  dplyr::select(c("V000", "V005","V021","V022","V024", "V131", "S730F", "S730H"))

data_short$S730F <- as.numeric(data_short$S730F)
data_short$S730H <- as.numeric(data_short$S730H)

# recode fGM
data_short$fgm <- ifelse(data_short$S730F == 1, 1, data_short$S730H)
data_short$fgm <- ifelse(data_short$fgm == 1, 0, 1)


# create a complex sample survey object to calculate standard errors with taylor-series linearization
survey.data <- svydesign(id             = ~V021, 
                         strata         = ~V022, 
                         variables      = ~fgm + V024 + V131,
                         weight         = data_short$V005,
                         data           = data_short)

tmp <- as.data.frame(svyby(~fgm, ~V024, survey.data, svymean, na.rm = TRUE))

tmp$country <- data$V000[1]

results.fgm <- rbind(results.fgm,setnames(tmp,names(results.fgm)))


# Prepare and outputfile called "results.dhs.csv) -------------------------
names(results.fgm) <- c("region", "fgm", "se", "country")

# Output file as CSV
setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Output")
write.csv(results.fgm, "results.dhs.csv")

# MICS -----------------------------------------------------------------
results.mics <- data.frame(matrix(NA, nrow = 0, ncol = 4))

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Surveys")
listsav.mics <- dir(pattern = "*.sav")
listsav.mics <- listsav.mics[-c(6, 9)] #remove Sierra Leone and Mali as different data labels

# start loop through standardized MICS surveys
for(i in 1:length(listsav.mics)){

  data <- read.spss(listsav.mics[i], to.data.frame=TRUE) 

  # select variables and create dataset for birth reshape and FGM reshape
  data_short <- data %>%
    dplyr::select(c("wmweight","HH6","HH1","HH7", "FG1", "FG3"))
  
  # add strata
  data_short$V022 <- paste(data_short$HH6,data_short$HH7)
  
  data_short$FG1 <- as.numeric(data_short$FG1)
  data_short$FG3 <- as.numeric(data_short$FG3)
  table(data_short$FG1)
  table(data_short$FG3)
  
  # No = 2 | Yes = 1 | 3 = No Response
  # recode FGM
  data_short$fgm <- ifelse(data_short$FG1 == 2, 2, data_short$FG3)
  data_short$fgm <- ifelse(data_short$fgm == 2, 0, 1)
  
  # create a complex sample survey object to calculate standard errors with taylor-series linearization
  survey.data <- svydesign(id             = ~HH1, 
                           strata         = ~V022, 
                           variables      = ~fgm + HH7,
                           weight         = data_short$wmweight,
                           data           = data_short)
  
  
  tmp <- as.data.frame(svyby(~fgm, ~HH7, survey.data, svymean, na.rm = TRUE))
  tmp$country <- listsav.mics[i]
  results.mics <- rbind(results.mics,setnames(tmp,names(results.mics)))
}

# MALI
data <- read.spss("Mali.mics.sav", to.data.frame=TRUE) 

# select variables and create dataset for birth reshape and FGM reshape
data_short <- data %>%
  dplyr::select(c("wmweight","HH6","WM1","HH7", "FG1", "FG3"))

# add strata
data_short$V022 <- paste(data_short$HH6,data_short$HH7)

data_short$FG1 <- as.numeric(data_short$FG1)
data_short$FG3 <- as.numeric(data_short$FG3)

# recode fGM
data_short$fgm <- ifelse(data_short$FG1 == 2, 2, data_short$FG3)
data_short$fgm <- ifelse(data_short$fgm == 2, 0, 1)

# create a complex sample survey object to calculate standard errors with taylor-series linearization
survey.data <- svydesign(id             = ~WM1, 
                         strata         = ~V022, 
                         variables      = ~fgm + HH7,
                         weight         = data_short$wmweight,
                         data           = data_short)


tmp <- as.data.frame(svyby(~fgm, ~HH7, survey.data, svymean, na.rm = TRUE))
tmp$country <- "Mali"

results.mics <- rbind(results.mics,setnames(tmp,names(results.mics)))

# Sierra Leone

data <- read.spss("Sierra Leone.mics.sav", to.data.frame=TRUE) 

# select variables and create dataset for birth reshape and FGM reshape
data_short <- data %>%
  dplyr::select(c("wmweight","HH6","WM1","HH7","HH7A", "FG1", "FG3"))

# add strata
data_short$V022 <- paste(data_short$HH6,data_short$HH7)


table(data_short$FG1)
table(data_short$FG3)

data_short$FG1 <- as.numeric(data_short$FG1)
data_short$FG3 <- as.numeric(data_short$FG3)

# recode fGM
data_short$fgm <- ifelse(data_short$FG1 == 2, 2, data_short$FG3)
data_short$fgm <- ifelse(data_short$fgm == 2, 0, 1)


# create a complex sample survey object to calculate standard errors with taylor-series linearization
survey.data <- svydesign(id             = ~WM1, 
                         strata         = ~V022, 
                         variables      = ~fgm + HH7A,
                         weight         = data_short$wmweight,
                         data           = data_short)


tmp <- as.data.frame(svyby(~fgm, ~HH7A, survey.data, svymean, na.rm = TRUE))
tmp$country <- "Sierra Leone"

results.mics <- rbind(results.mics,setnames(tmp,names(results.mics)))

# Prepare and outputfile called "results.dhs.csv) -------------------------
names(results.mics) <- c("region", "fgm", "se", "country")

# Output file as CSV
setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Output")
write.csv(results.mics,'results.mics.csv')
