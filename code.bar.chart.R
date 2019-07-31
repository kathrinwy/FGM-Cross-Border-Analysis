
library(readxl)
library(tidyverse)
library(ggplot2)

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Descriptive statistics")
results <-read.csv("ethnicities.csv") # load subnational prevalence rates for fgm women 15-49


# Kenya -------------------------------------------------------------------

data <- results %>%
  filter(country == "Kenya")

ggplot(data, aes(x=reorder(ethnicity, -data), y= data)) +
  geom_bar(stat="identity")+
  coord_flip() +
  theme_classic()