
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(raster)
library(tidyverse)
library(survey)
library(foreign)
library(reshape2)
library(survival)
library(survminer)
library(gtable)
library(ggthemes)
library(xlsx)

# Load data ---------------------------------------------------------------

setwd("G:/My Drive/2019/1- FGM/07- FGM and Ethnicity/Eastern Africa/Data")

# Load data ---------------------------------------------------------------

survival_data <- readRDS("survival_data.rds", refhook = NULL)

survival_data$re_wgt <- as.numeric(survival_data$re_wgt) # weights need to be numeric
survival_data$time <- as.numeric(survival_data$time) # weights need to be numeric
survival_data$fgm <- as.numeric(survival_data$fgm) # weights need to be numeric

# Set up data set --------------------------------------------------

survival_data$cohort5 <- as.factor(survival_data$cohort5)
survival_data$cohort5 = factor(survival_data$cohort5,levels(survival_data$cohort5)[c(5:12, 2:4)]) 
survival_data$cohort5 <- relevel(survival_data$cohort5, ref = 2)
survival_data$cohort5 <- ordered(survival_data$cohort5,
                                 levels = c(2:12),
                                 labels = c("1960 -1964",
                                            "1965 -1969",
                                            "1970 -1974", 
                                            "1975 -1979",
                                            "1980 -1984",
                                            "1985 -1989",
                                            "1990 -1994", 
                                            "1995 -1999",
                                            "2000 -2004", 
                                            "2005 -2009",
                                            "after 2009"))

survival_data$cohort10 <- as.factor(survival_data$cohort10)
survival_data$cohort10 <- ordered(survival_data$cohort10,
                                  levels = c(1:6),
                                  labels = c("1960 -1969",
                                             "1970 -1979",
                                             "1980 -1989",
                                             "1990 -1999", 
                                             "2000 -2009", 
                                             "after 2009")) 

survival_data <- survival_data %>%
  filter(!is.na(fgm)) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(cohort5))%>%
  filter(!is.na(cohort10)) %>%
  filter(time != 98)%>%
  filter(time != 99)

# 11 observations where age at cutting > than age of girl in years
survival_data <- survival_data%>%
  filter(as.numeric(time) <= as.numeric(age_years))

# Figure 2, 5, 7, 8 and 9 (age at FGM survival curves) --------------------
setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Visualization")

# Only keep latest survey
data.latest <- survival_data %>%
  filter(country == "Ethiopia" |
        (country == "Kenya" & year == 2014) |
         country == "Somalia NE Zone" |
         country == "Somaliland"|
        (country == "Tanzania" & (year == 2015 | year == 2016)))

list.countries <- c("Ethiopia", "Kenya", "Tanzania", "Somalia NE Zone", "Somaliland")

for(i in list.countries){
  i = "Kenya"
  data <- data.latest %>%
    filter(country == i) %>%
    filter(fgm >0) 
  
  survival.curve <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~1, 
                            data,
                            weight= as.numeric(re_wgt))
  
  mc <- data.frame(q = c(.25, .5, .75),
                   km = quantile(survival.curve))
  
  temp.results <- data.frame(i, t(as.data.frame(quantile(survival.curve)[1])))
  
  survplot <- ggsurvplot(survival.curve, xlab = "Time (years)", ylab = "Cumulative event",
                         censor = F, data, palette = "brown", conf.int = F, 
                         font.title=c(24, "bold", "brown"),
                         xlim=c(0,20))$plot +
    geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
    geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2) +
    scale_x_continuous(breaks = seq(0, 20, 1))+
    
    labs(title = data$country[1], subtitle = paste("Source:", data$survey[1],data$year[1]))+
    
    theme(legend.position = "none", plot.subtitle = element_text(size = 8))
  
  ggsave(file = paste(i, "_age-at-FGM.png"), print(survplot))
  
}

# Figure 3 (Ethioipia prevalence by ethnicity) and Figure 6 (Kenya) ---------

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



# Figure 6: Kenya age at cutting by ethnicity -----------------------------

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Visualization")

data <- survival_data %>%
  filter(country == "Kenya" & year == 2014)

data <- data %>%
  filter(fgm >0)

ethnicities <- c("Maasai", "Kisii", "Somali", "Samburu", "Meru", "Kalenjin", "Taita/Taveeta", "Embu")


for(i in ethnicities){
  
  temp <- data %>%
  filter(ethnicity == i)

  survival.curve <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~1, 
                            temp,
                            weight= as.numeric(re_wgt))
  
  mc <- data.frame(q = c(.25, .5, .75),
                   km = quantile(survival.curve))
  
  temp.results <- data.frame(i, t(as.data.frame(quantile(survival.curve)[1])))
  
  survplot <- ggsurvplot(survival.curve, xlab = "Age", ylab = "Probability of not experiencing FGM",
                         censor = F, data, palette = "brown", conf.int = F, 
                         font.title=c(24, "bold", "brown"),
                         xlim=c(0,20))$plot +
    geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
    geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2) +
    scale_x_continuous(breaks = seq(0, 20, 1))+
    
    labs(title = paste(data$country[1],":", i), 
         subtitle = paste("Source:", data$survey[1],data$year[1]))+
    
    theme(legend.position = "none", plot.subtitle = element_text(size = 8))
  
  ggsave(file = paste(data$country[1], i, "_age-at-FGM.png"), print(survplot))

}

# Figure 4: Ethiopia age at cutting by ethnicity -----------------------------

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Visualization")

data <- survival_data %>%
  filter(country == "Ethiopia")

data <- data %>%
  filter(fgm >0)

ethnicities <- c("Affar", "Hadiya", "Welaita", "Sidama", "Guragie", "Amhara", "Tigrie", "Somalie")

for(i in ethnicities){
  
  temp <- data %>%
    filter(ethnicity == i)
  
  survival.curve <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~1, 
                            temp,
                            weight= as.numeric(re_wgt))
  
  mc <- data.frame(q = c(.25, .5, .75),
                   km = quantile(survival.curve))
  
  temp.results <- data.frame(i, t(as.data.frame(quantile(survival.curve)[1])))
  
  survplot <- ggsurvplot(survival.curve, xlab = "Age", ylab = "Probability of not experiencing FGM",
                         censor = F, data, palette = "brown", conf.int = F, 
                         font.title=c(24, "bold", "brown"),
                         xlim=c(0,20))$plot +
    geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
    geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2) +
    scale_x_continuous(breaks = seq(0, 20, 1))+
    
    labs(title = paste(data$country[1],":", i), 
         subtitle = paste("Source:", data$survey[1],data$year[1]))+
    
    theme(legend.position = "none", plot.subtitle = element_text(size = 8))
  
  ggsave(file = paste( data$country[1],i, "_age-at-FGM.png"), print(survplot))
  
}



# Figure 9: Somalis in Kenya/Ethiopia/Somalia -----------------------------

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Visualization")

# Only keep latest survey
data.latest <- survival_data %>%
  filter(ethnicity == "Somali" | ethnicity == "Somalie") %>%
  filter(country == "Ethiopia" |
           (country == "Kenya" & year == 2014) |
           country == "Somalia NE Zone" |
           country == "Somaliland"|
           (country == "Tanzania" & (year == 2015 | year == 2016)))


survival.curve <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~ country, 
                          data.latest,
                          weight= as.numeric(re_wgt))

survplot <- ggsurvplot(survival.curve, xlab = "Age", ylab = "Probability of not experiencing FGM",
                       censor = F, data, conf.int = F, palette = "Dark2",
                       font.title=c(24, "bold", "brown"),
                       xlim=c(0,20),   
                       legend.labs = c("Ethiopia (DHS 2016)", "Kenya (DHS 2014)", 
                                       "Somalia NE Zone (MICS 2011)", "Somaliland (MICS 2011)"))$plot  +
  scale_x_continuous(breaks = seq(0, 20, 1))+
  
  labs(title = "Somali etchnicty across Ethiopia, Kenya and Somalia", color = "Country")+
  
  theme(legend.position = c(0.8, 0.4) , plot.subtitle = element_text(size = 8), plot.title = element_text(size=16),
        legend.title =element_text(face="bold")) 


ggsave(file = paste( "Figure9.png"), print(survplot))



