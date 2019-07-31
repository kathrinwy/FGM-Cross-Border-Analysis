
library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Descriptive statistics")
results <-read.csv("ethnicities.csv") # load subnational prevalence rates for fgm women 15-49


# Kenya -------------------------------------------------------------------

data <- results %>%
  filter(country == "Kenya")

ggplot(data, aes(x=reorder(ethnicity, -data), y= data)) +
  geom_bar(stat="identity", fill = "slategray1", color = "slategray")+
  theme_void()+
  labs(title = "FGM prevalence in Kenya, by ethnicity (DHS 2014)",
       y = "Percentage of women age 15-49 who experienced FGM",
       x = "")+
  geom_text(aes(label = paste(ethnicity, percent(data)), size = data), 
           position = position_dodge(0.9), angle = 90,
           hjust=ifelse(data$data < 0.18,1.1, -0.1))+
  scale_size(range = c(3.5, 6.5))+
  theme(legend.position = "none")


# Ethiopia ----------------------------------------------------------------

data <- results %>%
  filter(country == "Ethiopia")

ggplot(data, aes(x=reorder(ethnicity, -data), y= data)) +
  geom_bar(stat="identity", fill = "wheat1", color = "wheat4")+
  theme_void()+
  labs(title = "FGM prevalence in Ethiopia, by ethnicity (DHS 2016)",
       y = "Percentage of women age 15-49 who experienced FGM",
       x = "")+
  geom_text(aes(label = paste(ethnicity, percent(data)), size = data), 
            position = position_dodge(0.9), angle = 90,
            hjust=ifelse(data$data < 0.99,1.1, -0.1))+
  scale_size(range = c(4.5, 6.5))+
  theme(legend.position = "none")


