# Code written by Kathrin Weny

# last update 30 May  2019

# load necessary libraries
library(stringr)
library(tmap)
library(rgeos)
library(ggmap)
library(mapdata)
library(maptools)
library(maps)
library(leaflet)
library(rio)
library(tmaptools)
library(rgdal)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(raster)
library(tidyverse)
library(broom)
library(ggrepel)
library(scales)
library(grid)
library(sf)
library(openxlsx)


setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/FGM-Cross-Border-Analysis/Data")
results <-read.csv("results.csv") # load subnational prevalence rates for fgm women 15-49
results.national <- read.csv("fgm_prevalence.csv") # load national prevalence rates for fgm women 15-49

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/Boundaries")
geo <- readOGR(".", "Export_Output") # load global shapefile for  DHS data

subset <- geo[(geo@data$CNTRYNAMEE=="Benin" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Burkina Faso" & geo@data$SVYYEAR == 2010) |
                (geo@data$CNTRYNAMEE=="Chad" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Cote d'Ivoire" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Egypt" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Eritrea" & geo@data$SVYYEAR == 2002) |
                (geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="The Gambia" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Ghana" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Guinea" & geo@data$SVYYEAR == 2012) |
                #(geo@data$CNTRYNAMEE=="Kenya" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Liberia" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Mali" & geo@data$SVYYEAR == 2015) |
                (geo@data$CNTRYNAMEE=="Niger" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Nigeria" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Senegal" & geo@data$SVYYEAR == 2017) |
                (geo@data$CNTRYNAMEE=="Sierra Leone" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2015) |
                (geo@data$CNTRYNAMEE=="Togo" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Yemen" & geo@data$SVYYEAR == 2013),]

mau <- readOGR(".", "Mauritania_adm1")
gbi <- readOGR(".", "Guinea Bissau_adm1")
car <- readOGR(".", "Central African Republic_adm1")
sud <- readOGR(".", "Sudan_adm1")
ira <- readOGR(".", "iraq_admbnda_adm1")
som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")

mau@data$ID <- paste(mau@data$admin0Name, mau@data$admin1Name, sep="" )
gbi@data$ID <- paste(gbi@data$CNTRY_NAME, gbi@data$ADM1_NAME, sep="" )
car@data$ID <- paste(car@data$admin0Name, car@data$admin1Name, sep="" )
sud@data$ID <- paste(sud@data$admin0Name, sud@data$admin1Name, sep="" )
ira@data$ID <- paste(ira@data$admin0Name, ira@data$admin1Name, sep="" )
som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")

mau@data$ID2 <- paste(mau@data$admin0Name)
gbi@data$ID2 <- paste(gbi@data$CNTRY_NAME)
car@data$ID2 <- paste(car@data$admin0Name)
sud@data$ID2 <- paste(sud@data$admin0Name)
ira@data$ID2 <- paste(ira@data$admin0Name)
som@data$ID2 <- paste(som@data$admin0Name)
ken@data$ID2 <- paste(ken@data$CNTRYNAMEE)

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )
subset@data$ID2 <- paste(subset@data$CNTRYNAMEE)

x <- raster::bind(mau, subset)
y <- raster::bind(gbi, x)
z <- raster::bind(car, y)
k <- raster::bind(sud, z)
l <- raster::bind(ira, k)
m <- raster::bind(som, l)
n <- raster::bind(ken, m)

subset <- n

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID, ID2)%>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID2 = as.character(ID2))

data <- results %>%
  dplyr::select(c("ID", "fgm"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping_subnational <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping_subnational$ID))
a <- as.data.frame(subset@data$ID)

# select correct level of disaggregation
mapping_subnational <-mapping_subnational %>% 
  filter(ID != "GuineaLower Guinea" |
           ID != "GuineaCentral Guinea" |
           ID != "GuineaUpper Guinea" |
           ID != "GuineaForest Guinea")

mapping_subnational$Data <- as.numeric(mapping_subnational$Data)

# Maps

plot1.green.red <- ggplot(data=mapping_subnational, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  borders("world", col = "gray27", lwd = 0.5)+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-49, %", low = "green3", high = "firebrick3",
                      labels = percent) +
  theme(legend.position = c(0.10,0.2), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 8))

plot1a <- ggplot(data=mapping_subnational, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  borders("world", col = "gray27", lwd = 0.5)+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-49, %", low = "yellow", high = "red",
                      labels = percent) +
  theme(legend.position = c(0.10,0.2), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 8))

plot1b <- ggplot(data=mapping_subnational, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  borders("world", col = "gray27", lwd = 0.5)+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-49, %", low = "burlywood1", high = "brown4",
                      labels = percent) +
  theme(legend.position = c(0.10,0.2), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 8))




# Map 2

world_map <- map_data("world")

plot1 <- ggplot()+
  geom_polygon(world_map, mapping = aes(x = long, y = lat, group = group), fill="azure3")+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  theme_void()+
  labs(fill = " ")

plot2.yellow.red <- plot1+
  geom_polygon(data=mapping_subnational, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-49, %", low = "yellow", high = "red",
                      labels = percent, na.value = "azure3") +
  borders("world", col = "white", lwd = 0.5)+
  theme(legend.position = c(0.15,0.2), plot.background = element_rect(fill = "slategray1"))

plot2.yellow.red.no.legend <- plot1+
  geom_polygon(data=mapping_subnational, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-49, %", low = "yellow", high = "red",
                      labels = percent, na.value = "azure3") +
  borders("world", col = "white", lwd = 0.5)+
  theme(legend.position = "none", plot.background = element_rect(fill = "slategray1"))


# Map 3
plot3.blues     <- ggplot(data=mapping_subnational, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  borders("world", col = "gray27", lwd = 0.5)+
  coord_equal(ylim=c(-12,40), xlim=c(-20,50))+
  scale_fill_gradient( name = "FGM prevalence \n Women 15-49, %") +
  theme(legend.position = c(0.15,0.2), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 8))

plot3.zoom.East.Africa   <- ggplot(data=mapping_subnational,
                                   mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  borders("world", col = "gray27", lwd = 1.5)+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-49, %", high="red", low="yellow") +
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  theme(legend.position = "none")+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 12))

# Create country labels
grob.eth <- grobTree(textGrob("Ethiopia", x=0.4,  y=0.85, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="italic")))
grob.som <- grobTree(textGrob("Somaliland", x=0.58,  y=0.78, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="italic")))
grob.sne <- grobTree(textGrob("Somalia \n Northeast Zone", x=0.8,  y=0.78, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="italic")))
grob.ken <- grobTree(textGrob("Kenya", x=0.376,  y=0.525, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="italic")))
grob.uga <- grobTree(textGrob("Uganda", x=0.225,  y=0.5, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="italic")))
grob.tan <- grobTree(textGrob("Tanzania", x=0.27,  y=0.3, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="italic")))

Zoom.East.Africa <- plot3.zoom.East.Africa + annotation_custom(grob.eth) +
  annotation_custom(grob.som) +
  annotation_custom(grob.sne) +
  annotation_custom(grob.ken) +
  annotation_custom(grob.uga) +
  annotation_custom(grob.tan)

# Export global map in vector format --------------------------------------

svg("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/global.map.red.green.svg",width=14,height=7)

plot1.green.red

dev.off()

svg("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/global.map.yellow.red.svg",width=14,height=7)

plot1a

dev.off()

svg("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/global.map.brown.blue.svg",width=14,height=7)

plot1b

dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/global.map.png", width = 16, height = 16, units = 'in', res = 600)

plot1.green.red

dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/subnational.map.with.legend.png", width = 16, height = 16, units = 'in', res = 600)

plot2.yellow.red 

dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/subnational.map.png", 
    width = 16, height = 16, units = 'in', res = 600)

plot2.yellow.red.no.legend 

dev.off()

svg("G:/My Drive/2019/1- FGM/05- Country profiles/Map/Zoom.East.Africa.svg", width = 10, height = 9, units = 'in', res = 600)

Zoom.East.Africa

dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/subnational.mapEastAfrica.png", 
    width = 10, height = 9, units = 'in', res = 600)

Zoom.East.Africa

dev.off()

jpeg("Plot3.jpeg",   width = 10, height = 9, units = 'in', res = 600)

Zoom.East.Africa

dev.off()

getwd()
# Ethnicities - plot ADMIN1 with majority Somali population  --------------

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016),]
subset <- subset[(subset@data$DHSREGEN == "Somali"),]
som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")
ken <- ken[(ken@data$DHSREGEN == "North Eastern"),]

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

x <- raster::bind(subset, som)
y <- raster::bind(ken, x)

subset <- y

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

results <-read.csv("resultsIII.csv")

data <- results %>%
  dplyr::select(c("ID", "somali"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match$ID <- as.character(data_match$ID)

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping2 <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

# Plot baseline layer: FGM prevalence by ADMIN1
p1 <-  ggplot(data=mapping,
              mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  scale_fill_gradient( name = "FGM prevalence \n Women 15-49, %", 
                       low = "green3", high = "firebrick3", labels = percent,
                       na.value = "gray34") +
  theme(legend.position = "none")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))

# Plot second layer: Borders around ADMIN1 layers who are majority Somali
ct_sf <- st_as_sf(subset)
unique(subset@data$ID)
somali <- ct_sf %>%
  filter(
    ID %in% c(
      "SomaliaAwdal", "EthiopiaSomali", "SomaliaNugaal","SomaliaMudug", "SomaliaLower Juba", "SomaliaMiddle Juba",
      "SomaliaWoqooyi Galbeed", "SomaliaSool", "SomaliaLower Shabelle" , "SomaliaMiddle Shabelle",  "SomaliaSanaag",  
      "SomaliaTogdheer",  "SomaliaSool", "SomaliaHiraan", "SomaliaGedo",  "SomaliaGalgaduud", 
      "SomaliaBay", "SomaliaBari", "SomaliaBanadir", "SomaliaBakool", "SomaliaAwdal", "KenyaNorth Eastern"  )
  ) %>%
  summarise(id = "somali")

# Merge basline layer and second layer and draw thrid layer: ADMIN0 boundaries
p1 +
  borders("world", col = "azure3", lwd =0.75)+ # third layer: ADMIN0 boundaries
  geom_sf(data =   somali, fill = "transparent", color = "blue", size=1, inherit.aes=  F)+
  coord_sf(ylim=c(-12,15), xlim=c(25,57), expand = FALSE)+
  theme(panel.grid.major = element_line(color = "white",
                                        linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "white"))

# Some manual cleainng is necessary due to inconsistencies between boundaries

# Map for Maasai - needs Kenya Admin2

subset <- geo[(geo@data$CNTRYNAMEE=="Benin" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Burkina Faso" & geo@data$SVYYEAR == 2010) |
                (geo@data$CNTRYNAMEE=="Chad" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Cote d'Ivoire" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Egypt" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Eritrea" & geo@data$SVYYEAR == 2002) |
                (geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="The Gambia" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Ghana" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Guinea" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Kenya" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Liberia" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Mali" & geo@data$SVYYEAR == 2015) |
                (geo@data$CNTRYNAMEE=="Niger" & geo@data$SVYYEAR == 2012) |
                (geo@data$CNTRYNAMEE=="Nigeria" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Senegal" & geo@data$SVYYEAR == 2017) |
                (geo@data$CNTRYNAMEE=="Sierra Leone" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2015) |
                (geo@data$CNTRYNAMEE=="Togo" & geo@data$SVYYEAR == 2013) |
                (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Yemen" & geo@data$SVYYEAR == 2013),]

mau <- readOGR(".", "Mauritania_adm1")
gbi <- readOGR(".", "Guinea Bissau_adm1")
car <- readOGR(".", "Central African Republic_adm1")
sud <- readOGR(".", "Sudan_adm1")
ira <- readOGR(".", "iraq_admbnda_adm1")
som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")


mau@data$ID <- paste(mau@data$admin0Name, mau@data$admin1Name, sep="" )
gbi@data$ID <- paste(gbi@data$CNTRY_NAME, gbi@data$ADM1_NAME, sep="" )
car@data$ID <- paste(car@data$admin0Name, car@data$admin1Name, sep="" )
sud@data$ID <- paste(sud@data$admin0Name, sud@data$admin1Name, sep="" )
ira@data$ID <- paste(ira@data$admin0Name, ira@data$admin1Name, sep="" )
som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )


subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

x <- raster::bind(mau, subset)
y <- raster::bind(gbi, x)
z <- raster::bind(car, y)
k <- raster::bind(sud, z)
l <- raster::bind(ira, k)
m <- raster::bind(som, l)


subset <- m

ct_sf <- st_as_sf(subset)


maasai <- ct_sf %>%
  filter(
    ID %in% c(
      "KenyaKajiado", "KenyaNarok", "TanzaniaArusha" )
  ) %>%
  summarise(id = "maasai")


p1 +
  
  borders("world", col = "azure3", lwd =0.75)+
  
  geom_sf(data =   maasai, fill = "transparent", color = "blue", size=1, inherit.aes=  F)+
  
  coord_sf(ylim=c(-12,15), xlim=c(25,57), expand = FALSE)+
  
  theme(panel.grid.major = element_line(color = "white",
                                        linetype = "dashed", size = 0.5), 
        
        panel.background = element_rect(fill = "white"))


# Horn of Africa Map ------------------------------------------------------

setwd("G:/My Drive/2019/1- FGM/10- Cross-border Analysis/FGM-Cross-Border/Data")

# Wealth -----------------------------------------------------------------

wealth    <- read.csv("wealth.csv") %>%
  dplyr::select(c("region", "wealth", "country"))

wealth$region <- as.character(wealth$region)

wealth$region <- ifelse(wealth$region == "Afar", "Affar", wealth$region)
wealth$region <- ifelse(wealth$region == "SNNPR", "SNNP", wealth$region)
wealth$region <- ifelse(wealth$region == " Morogoro", "Morogoro", wealth$region)
wealth$region <- ifelse(wealth$region == " Pwani", "Pwani", wealth$region)
wealth$region <- ifelse(wealth$region == " Dar Es Salaam", "Dar es Salaam", wealth$region)
wealth$region <- ifelse(wealth$region == " Kilimanjaro", "Kilimanjaro", wealth$region)
wealth$region <- ifelse(wealth$region == " Tanga", "Tanga", wealth$region)
wealth$region <- ifelse(wealth$region == "Oromia", "Oromiya", wealth$region)
wealth$region <- ifelse(wealth$region == "Addis Adaba", "Addis Abeba", wealth$region)
wealth$region <- ifelse(wealth$region == "Nugal", "Nugaal", wealth$region)
wealth$region <- ifelse(wealth$region == " Arusha", "Arusha", wealth$region)
wealth$region <- ifelse(wealth$region == "Benishangul", "Ben-Gumz", wealth$region)
wealth$region <- ifelse(wealth$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", wealth$region)
wealth$region <- ifelse(wealth$region == "Central1", "Central 1", wealth$region)
wealth$region <- ifelse(wealth$region == "Central2", "Central 2", wealth$region)
wealth$region <- ifelse(wealth$region == "Muranga", "Murang'a", wealth$region) 
wealth$region <- ifelse(wealth$region == "Elgeyo Marak", "Elgeyo Marakwet", wealth$region) 
wealth$region <- ifelse(wealth$region == "Tharaka", "Tharaka-Nithi", wealth$region) 

wealth$ID <- paste0(wealth$country, wealth$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                # (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2015),]


som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")
uga <- readOGR(".", "Uganda_sdr_subnational_boundaries")


som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")
uga@data$ID <- paste(uga@data$CNTRYNAMEE, uga@data$DHSREGEN, sep="")

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)
n <- raster::bind(ken, m)
o <- raster::bind(uga, n)

subset <- o

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- wealth %>%
  dplyr::select(c("ID", "wealth"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)


# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Wealth quintile")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of women (15-49) \n in lowest wealth quintile", 
                       low = "darkblue", high = "deeppink",
                       na.value = "grey50", limits=c(0, 1)) +
  
  borders("world")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 10))+
  
  theme(plot.title = element_text( face = "bold", size = (20)),
        legend.position = c(0.95,0.35), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 18))

# Education -----------------------------------------------------------------

education <-read.csv("education.csv")%>%
  dplyr::select(c("region", "education", "country"))

education$region <- as.character(education$region)

education$region <- ifelse(education$region == "Afar", "Affar", education$region)
education$region <- ifelse(education$region == "SNNPR", "SNNP", education$region)
education$region <- ifelse(education$region == " Morogoro", "Morogoro", education$region)
education$region <- ifelse(education$region == " Pwani", "Pwani", education$region)
education$region <- ifelse(education$region == " Dar Es Salaam", "Dar es Salaam", education$region)
education$region <- ifelse(education$region == " Kilimanjaro", "Kilimanjaro", education$region)
education$region <- ifelse(education$region == " Tanga", "Tanga", education$region)
education$region <- ifelse(education$region == "Oromia", "Oromiya", education$region)
education$region <- ifelse(education$region == "Addis Adaba", "Addis Abeba", education$region)
education$region <- ifelse(education$region == "Nugal", "Nugaal", education$region)
education$region <- ifelse(education$region == " Arusha", "Arusha", education$region)
education$region <- ifelse(education$region == "Benishangul", "Ben-Gumz", education$region)
education$region <- ifelse(education$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", education$region)
education$region <- ifelse(education$region == "Central1", "Central 1", education$region)
education$region <- ifelse(education$region == "Central2", "Central 2", education$region)
education$region <- ifelse(education$region == "Muranga", "Murang'a", education$region) 
education$region <- ifelse(education$region == "Elgeyo Marak", "Elgeyo Marakwet", education$region) 
education$region <- ifelse(education$region == "Tharaka", "Tharaka-Nithi", education$region) 

education$ID <- paste0(education$country, education$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Kenya" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2017),]

som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)

subset <- m

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- education %>%
  dplyr::select(c("ID", "education"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

test <- mapping %>%
  filter(id == "110")

b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)
a.1 <- as.data.frame(subset@data$CNTRYNAMEE)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Education")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of women (15-49) \n with no education", 
                       low = "darkblue", high = "green",
                       na.value = "grey50", limits=c(0, 1)) +
  
  borders("world")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 10))+
  
  theme(plot.title = element_text( face = "bold", size = (20)),
        legend.position = c(0.95,0.35), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 18))


# Residence -----------------------------------------------------------------

residence <-read.csv("residence.csv") %>%
  dplyr::select(c("region", "residence", "country"))

residence$region <- as.character(residence$region)

residence$region <- ifelse(residence$region == "Afar", "Affar", residence$region)
residence$region <- ifelse(residence$region == "SNNPR", "SNNP", residence$region)
residence$region <- ifelse(residence$region == " Morogoro", "Morogoro", residence$region)
residence$region <- ifelse(residence$region == " Pwani", "Pwani", residence$region)
residence$region <- ifelse(residence$region == " Dar Es Salaam", "Dar es Salaam", residence$region)
residence$region <- ifelse(residence$region == " Kilimanjaro", "Kilimanjaro", residence$region)
residence$region <- ifelse(residence$region == " Tanga", "Tanga", residence$region)
residence$region <- ifelse(residence$region == "Oromia", "Oromiya", residence$region)
residence$region <- ifelse(residence$region == "Addis Adaba", "Addis Abeba", residence$region)
residence$region <- ifelse(residence$region == "Nugal", "Nugaal", residence$region)
residence$region <- ifelse(residence$region == " Arusha", "Arusha", residence$region)
residence$region <- ifelse(residence$region == "Benishangul", "Ben-Gumz", residence$region)
residence$region <- ifelse(residence$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", residence$region)
residence$region <- ifelse(residence$region == "Central1", "Central 1", residence$region)
residence$region <- ifelse(residence$region == "Central2", "Central 2", residence$region)
residence$region <- ifelse(residence$region == "Muranga", "Murang'a", residence$region) 
residence$region <- ifelse(residence$region == "Elgeyo Marak", "Elgeyo Marakwet", residence$region) 
residence$region <- ifelse(residence$region == "Tharaka", "Tharaka-Nithi", residence$region) 

residence$ID <- paste0(residence$country, residence$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Kenya" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2017),]

som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)

subset <- m

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- residence %>%
  dplyr::select(c("ID", "residence"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Rural residence")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of women (15-49) \n living in rural areas", 
                       low = "orange", high = "darkblue",
                       na.value = "grey50", limits=c(0, 1)) +
  
  borders("world")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 10))+
  
  theme(plot.title = element_text( face = "bold", size = (20)),
        legend.position = c(0.95,0.35), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 18))


# Electricity -----------------------------------------------------------------

electricity <-read.csv("electricity.csv") %>%
  dplyr::select(c("region", "electricity", "country"))

electricity$region <- as.character(electricity$region)

electricity$region <- ifelse(electricity$region == "Afar", "Affar", electricity$region)
electricity$region <- ifelse(electricity$region == "SNNPR", "SNNP", electricity$region)
electricity$region <- ifelse(electricity$region == " Morogoro", "Morogoro", electricity$region)
electricity$region <- ifelse(electricity$region == " Pwani", "Pwani", electricity$region)
electricity$region <- ifelse(electricity$region == " Dar Es Salaam", "Dar es Salaam", electricity$region)
electricity$region <- ifelse(electricity$region == " Kilimanjaro", "Kilimanjaro", electricity$region)
electricity$region <- ifelse(electricity$region == " Tanga", "Tanga", electricity$region)
electricity$region <- ifelse(electricity$region == "Oromia", "Oromiya", electricity$region)
electricity$region <- ifelse(electricity$region == "Addis Adaba", "Addis Abeba", electricity$region)
electricity$region <- ifelse(electricity$region == "Nugal", "Nugaal", electricity$region)
electricity$region <- ifelse(electricity$region == " Arusha", "Arusha", electricity$region)
electricity$region <- ifelse(electricity$region == "Benishangul", "Ben-Gumz", electricity$region)
electricity$region <- ifelse(electricity$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", electricity$region)
electricity$region <- ifelse(electricity$region == "Central1", "Central 1", electricity$region)
electricity$region <- ifelse(electricity$region == "Central2", "Central 2", electricity$region)
electricity$region <- ifelse(electricity$region == "Muranga", "Murang'a", electricity$region) 
electricity$region <- ifelse(electricity$region == "Elgeyo Marak", "Elgeyo Marakwet", electricity$region) 
electricity$region <- ifelse(electricity$region == "Tharaka", "Tharaka-Nithi", electricity$region) 

electricity$ID <- paste0(electricity$country, electricity$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Kenya" & geo@data$SVYYEAR == 2014) |
                (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2017),]

som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)

subset <- m

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- electricity %>%
  dplyr::select(c("ID", "electricity"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Electricity")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of households \n with no electricity", 
                       low = "gold", high = "darkblue",
                       na.value = "grey50", limits=c(0, 1)) +
  
  borders("world")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 10))+
  
  theme(plot.title = element_text( face = "bold", size = (20)),
        legend.position = c(0.92,0.35), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 18))


# Skilled Birth Attendance ------------------------------------------------

sba   <- read.csv("SBA.csv") %>%
  dplyr::select(c("region", "sba", "country"))

sba$region <- as.character(sba$region)

sba$region <- ifelse(sba$region == "Afar", "Affar", sba$region)
sba$region <- ifelse(sba$region == "SNNPR", "SNNP", sba$region)
sba$region <- ifelse(sba$region == " Morogoro", "Morogoro", sba$region)
sba$region <- ifelse(sba$region == " Pwani", "Pwani", sba$region)
sba$region <- ifelse(sba$region == " Dar Es Salaam", "Dar es Salaam", sba$region)
sba$region <- ifelse(sba$region == " Kilimanjaro", "Kilimanjaro", sba$region)
sba$region <- ifelse(sba$region == " Tanga", "Tanga", sba$region)
sba$region <- ifelse(sba$region == "Oromia", "Oromiya", sba$region)
sba$region <- ifelse(sba$region == "Addis Adaba", "Addis Abeba", sba$region)
sba$region <- ifelse(sba$region == "Nugal", "Nugaal", sba$region)
sba$region <- ifelse(sba$region == " Arusha", "Arusha", sba$region)
sba$region <- ifelse(sba$region == "Benishangul", "Ben-Gumz", sba$region)
sba$region <- ifelse(sba$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", sba$region)
sba$region <- ifelse(sba$region == "Central1", "Central 1", sba$region)
sba$region <- ifelse(sba$region == "Central2", "Central 2", sba$region)
sba$region <- ifelse(sba$region == "Muranga", "Murang'a", sba$region) 
sba$region <- ifelse(sba$region == "Elgeyo Marak", "Elgeyo Marakwet", sba$region) 
sba$region <- ifelse(sba$region == "Tharaka", "Tharaka-Nithi", sba$region) 

sba$ID <- paste0(sba$country, sba$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                # (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2015),]


som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")
uga <- readOGR(".", "Uganda_sdr_subnational_boundaries")

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")
uga@data$ID <- paste(uga@data$CNTRYNAMEE, uga@data$DHSREGEN, sep="")

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)
n <- raster::bind(ken, m)
o <- raster::bind(uga, n)

subset <- o

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- sba %>%
  dplyr::select(c("ID", "sba"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Skilled birth attendance")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of live births  \n assisted by a skilled provider \n (5 years preceding the survey)", 
                       low = "lightsalmon", high = "forestgreen",
                       na.value = "grey50", limits=c(0, 1)) +
  
  borders("world")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 10))+
  
  theme(plot.title = element_text( face = "bold", size = (20)),
        legend.position = c(0.95,0.35), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16))


# demand for family planning satisfied ------------------------------------------------

mpds   <- read.csv("mpds.csv") %>%
  dplyr::select(c("region", "mpds", "country"))

mpds$region <- as.character(mpds$region)

mpds$region <- ifelse(mpds$region == "Afar", "Affar", mpds$region)
mpds$region <- ifelse(mpds$region == "SNNPR", "SNNP", mpds$region)
mpds$region <- ifelse(mpds$region == " Morogoro", "Morogoro", mpds$region)
mpds$region <- ifelse(mpds$region == " Pwani", "Pwani", mpds$region)
mpds$region <- ifelse(mpds$region == " Dar Es Salaam", "Dar es Salaam", mpds$region)
mpds$region <- ifelse(mpds$region == " Kilimanjaro", "Kilimanjaro", mpds$region)
mpds$region <- ifelse(mpds$region == " Tanga", "Tanga", mpds$region)
mpds$region <- ifelse(mpds$region == "Oromia", "Oromiya", mpds$region)
mpds$region <- ifelse(mpds$region == "Addis Adaba", "Addis Abeba", mpds$region)
mpds$region <- ifelse(mpds$region == "Nugal", "Nugaal", mpds$region)
mpds$region <- ifelse(mpds$region == " Arusha", "Arusha", mpds$region)
mpds$region <- ifelse(mpds$region == "Benishangul", "Ben-Gumz", mpds$region)
mpds$region <- ifelse(mpds$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", mpds$region)
mpds$region <- ifelse(mpds$region == "Central1", "Central 1", mpds$region)
mpds$region <- ifelse(mpds$region == "Central2", "Central 2", mpds$region)
mpds$region <- ifelse(mpds$region == "Muranga", "Murang'a", mpds$region) 
mpds$region <- ifelse(mpds$region == "Elgeyo Marak", "Elgeyo Marakwet", mpds$region) 
mpds$region <- ifelse(mpds$region == "Tharaka", "Tharaka-Nithi", mpds$region) 

mpds$ID <- paste0(mpds$country, mpds$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                # (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2015),]


som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")
uga <- readOGR(".", "Uganda_sdr_subnational_boundaries")

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")
uga@data$ID <- paste(uga@data$CNTRYNAMEE, uga@data$DHSREGEN, sep="")

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)
n <- raster::bind(ken, m)
o <- raster::bind(uga, n)

subset <- o

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- mpds %>%
  dplyr::select(c("ID", "mpds"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Demand for Family Planning Satisfied")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of married or in union women  \n 15-49, who have their demand for \n family planning satsified", 
                       low = "lightsteelblue1", high = "mediumblue",
                       na.value = "grey50", limits=c(0, 1)) +
  
  borders("world")+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 10))+
  
  theme(plot.title = element_text( face = "bold", size = (20)),
        legend.position = c(0.95,0.35), legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12))


# FGM prevalence ------------------------------------------------

fgm   <- read.csv("resultsII.csv") %>%
  dplyr::select(c("Region", "fgm", "country"))

fgm$region <- as.character(fgm$Region)

fgm$region <- ifelse(fgm$region == "Afar", "Affar", fgm$region)
fgm$region <- ifelse(fgm$region == "SNNPR", "SNNP", fgm$region)
fgm$region <- ifelse(fgm$region == " Morogoro", "Morogoro", fgm$region)
fgm$region <- ifelse(fgm$region == " Pwani", "Pwani", fgm$region)
fgm$region <- ifelse(fgm$region == " Dar Es Salaam", "Dar es Salaam", fgm$region)
fgm$region <- ifelse(fgm$region == " Kilimanjaro", "Kilimanjaro", fgm$region)
fgm$region <- ifelse(fgm$region == " Tanga", "Tanga", fgm$region)
fgm$region <- ifelse(fgm$region == "Oromia", "Oromiya", fgm$region)
fgm$region <- ifelse(fgm$region == "Addis Adaba", "Addis Abeba", fgm$region)
fgm$region <- ifelse(fgm$region == "Nugal", "Nugaal", fgm$region)
fgm$region <- ifelse(fgm$region == " Arusha", "Arusha", fgm$region)
fgm$region <- ifelse(fgm$region == "Benishangul", "Ben-Gumz", fgm$region)
fgm$region <- ifelse(fgm$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", fgm$region)
fgm$region <- ifelse(fgm$region == "Central1", "Central 1", fgm$region)
fgm$region <- ifelse(fgm$region == "Central2", "Central 2", fgm$region)
fgm$region <- ifelse(fgm$region == "Muranga", "Murang'a", fgm$region) 
fgm$region <- ifelse(fgm$region == "Elgeyo Marak", "Elgeyo Marakwet", fgm$region) 
fgm$region <- ifelse(fgm$region == "Tharaka", "Tharaka-Nithi", fgm$region) 

fgm$ID <- paste0(fgm$country, fgm$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                # (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2017),]


som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")
uga <- readOGR(".", "Uganda_sdr_subnational_boundaries")

som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")
uga@data$ID <- paste(uga@data$CNTRYNAMEE, uga@data$DHSREGEN, sep="")

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)
n <- raster::bind(ken, m)
o <- raster::bind(uga, n)

subset <- o

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- fgm %>%
  dplyr::select(c("ID", "fgm"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

ggplot(data=mapping,
       mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  labs(title="FGM Prevalence (women 15-49)")+
  
  scale_fill_gradient( name = "FGM prevalence \n Women 15-49, %", 
                       low = "green3", high = "firebrick3", labels = percent,
                       na.value = "gray34", limits=c(0,1)) +
  borders("world", col = "gray27", lwd = 0.5)+
  coord_equal(ylim=c(-12,15), xlim=c(25,57))+
  theme(legend.position = c(0.85,0.35), legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16))+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 12))


# Wealth quintile national level ------------------------------------------

# Read in shapefiles

wealth    <- read.csv("wealth.csv") %>%
  dplyr::select(c("region", "wealth", "country"))

wealth$region <- as.character(wealth$region)

wealth$region <- ifelse(wealth$region == "Afar", "Affar", wealth$region)
wealth$region <- ifelse(wealth$region == "SNNPR", "SNNP", wealth$region)
wealth$region <- ifelse(wealth$region == " Morogoro", "Morogoro", wealth$region)
wealth$region <- ifelse(wealth$region == " Pwani", "Pwani", wealth$region)
wealth$region <- ifelse(wealth$region == " Dar Es Salaam", "Dar es Salaam", wealth$region)
wealth$region <- ifelse(wealth$region == " Kilimanjaro", "Kilimanjaro", wealth$region)
wealth$region <- ifelse(wealth$region == " Tanga", "Tanga", wealth$region)
wealth$region <- ifelse(wealth$region == "Oromia", "Oromiya", wealth$region)
wealth$region <- ifelse(wealth$region == "Addis Adaba", "Addis Abeba", wealth$region)
wealth$region <- ifelse(wealth$region == "Nugal", "Nugaal", wealth$region)
wealth$region <- ifelse(wealth$region == " Arusha", "Arusha", wealth$region)
wealth$region <- ifelse(wealth$region == "Benishangul", "Ben-Gumz", wealth$region)
wealth$region <- ifelse(wealth$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", wealth$region)
wealth$region <- ifelse(wealth$region == "Central1", "Central 1", wealth$region)
wealth$region <- ifelse(wealth$region == "Central2", "Central 2", wealth$region)
wealth$region <- ifelse(wealth$region == "Muranga", "Murang'a", wealth$region) 
wealth$region <- ifelse(wealth$region == "Elgeyo Marak", "Elgeyo Marakwet", wealth$region) 
wealth$region <- ifelse(wealth$region == "Tharaka", "Tharaka-Nithi", wealth$region) 

wealth$ID <- paste0(wealth$country, wealth$region)

subset <- geo[(geo@data$CNTRYNAMEE=="Ethiopia" & geo@data$SVYYEAR == 2016) |
                # (geo@data$CNTRYNAMEE=="Uganda" & geo@data$SVYYEAR == 2016) |
                (geo@data$CNTRYNAMEE=="Tanzania" & geo@data$SVYYEAR == 2015),]


som <- readOGR(".", "Som_Admbnda_Adm1_UNDP")
ken <- readOGR(".", "Kenya_subnational_boundaries")
uga <- readOGR(".", "Uganda_sdr_subnational_boundaries")


som@data$ID <- paste(som@data$admin0Name, som@data$admin1Name, sep="" )
ken@data$ID <- paste(ken@data$CNTRYNAMEE, ken@data$DHSREGEN, sep="")
uga@data$ID <- paste(uga@data$CNTRYNAMEE, uga@data$DHSREGEN, sep="")

subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )

m <- raster::bind(som, subset)
n <- raster::bind(ken, m)
o <- raster::bind(uga, n)

subset <- o

i <- "Somalia"

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID)%>%
  mutate(ID = as.character(ID))

data <- wealth %>%
  dplyr::select(c("ID", "wealth"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)


# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))

mapping <- dplyr::filter(mapping, grepl(i, ID))

p1 <- ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="Wealth quintile")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "Percentage of households \n in lowest wealth quintile", 
                       low = "darkblue", high = "deeppink", labels = percent,
                       na.value = "grey50",         limits = c(0,0.3), breaks = c(0, 0.1, 0.2, 0.3))+
  
  coord_equal()+
  
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 6))+
  
  theme(plot.title = element_text(face = "bold", size = (16)),
        legend.position = "right",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))


fgm   <- read.csv("resultsII.csv") %>%
  dplyr::select(c("Region", "fgm", "country"))

fgm$region <- as.character(fgm$Region)

fgm$region <- ifelse(fgm$region == "Afar", "Affar", fgm$region)
fgm$region <- ifelse(fgm$region == "SNNPR", "SNNP", fgm$region)
fgm$region <- ifelse(fgm$region == " Morogoro", "Morogoro", fgm$region)
fgm$region <- ifelse(fgm$region == " Pwani", "Pwani", fgm$region)
fgm$region <- ifelse(fgm$region == " Dar Es Salaam", "Dar es Salaam", fgm$region)
fgm$region <- ifelse(fgm$region == " Kilimanjaro", "Kilimanjaro", fgm$region)
fgm$region <- ifelse(fgm$region == " Tanga", "Tanga", fgm$region)
fgm$region <- ifelse(fgm$region == "Oromia", "Oromiya", fgm$region)
fgm$region <- ifelse(fgm$region == "Addis Adaba", "Addis Abeba", fgm$region)
fgm$region <- ifelse(fgm$region == "Nugal", "Nugaal", fgm$region)
fgm$region <- ifelse(fgm$region == " Arusha", "Arusha", fgm$region)
fgm$region <- ifelse(fgm$region == "Benishangul", "Ben-Gumz", fgm$region)
fgm$region <- ifelse(fgm$region == "Maroodijeex/Saaxil", "Woqooyi Galbeed", fgm$region)
fgm$region <- ifelse(fgm$region == "Central1", "Central 1", fgm$region)
fgm$region <- ifelse(fgm$region == "Central2", "Central 2", fgm$region)
fgm$region <- ifelse(fgm$region == "Muranga", "Murang'a", fgm$region) 
fgm$region <- ifelse(fgm$region == "Elgeyo Marak", "Elgeyo Marakwet", fgm$region) 
fgm$region <- ifelse(fgm$region == "Tharaka", "Tharaka-Nithi", fgm$region) 

fgm$ID <- paste0(fgm$country, fgm$region)

data <- fgm %>%
  dplyr::select(c("ID", "fgm"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)

# only plot eastern Africa
mapping$Data <- as.numeric(as.character(mapping$Data))
mapping <- dplyr::filter(mapping, grepl(i, ID))

p2 <- ggplot(data=mapping,mapping = aes(x=long, y=lat, group=group, fill = Data))+
  labs(title="FGM Prevalence (women 15-49)")+
  
  geom_polygon()+
  theme_void()+
  
  scale_fill_gradient( name = "FGM prevalence \n Women 15-49, %", 
                       low = "green3", high = "firebrick3", labels = percent,
                       na.value = "gray50",
                       limits = c(0.95,1), breaks=c(0.95, 0.975, 1)) +
  coord_equal()+
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 6))+
  
  theme(plot.title = element_text(face = "bold", size = (16)),
        legend.position = "right",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))


grid.arrange(p1, p2,  nrow=2)


# Map Ethiopia for ASRHR supplement ---------------------------------------

setwd("G:/My Drive/2019/1- FGM/11- Ad hoc tasks/ARHR supplement")

results <-read.csv("Ethiopia.csv") # load subnational prevalence rates for fgm women 15-49
geo <- readOGR(".", "sdr_subnational_boundaries") # load shapefile for  DHS data

subset <- geo
subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )
subset@data$ID2 <- paste(subset@data$CNTRYNAMEE)

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID, ID2)%>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID2 = as.character(ID2))

geo_data_1$id <- as.numeric(geo_data_1$id)-1

geo_data_1$id <- as.character(geo_data_1$id)

data <- results %>%
  dplyr::select(c("ID", "data", "year"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data", "Year") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

mapping$Data <- as.numeric(mapping$Data)

mapping$Fill <- ifelse(mapping$Data < 0.1, "less than 10%", 
                       ifelse(0.1 <= mapping$Data & mapping$Data < 0.2, "between 10% and 20%",
                              ifelse(0.2 <= mapping$Data & mapping$Data < 0.3, "between 20% and 30%",
                                     ifelse(0.3 <= mapping$Data & mapping$Data < 0.4, "between 30% and 40%",
                                            ifelse(0.4 <= mapping$Data & mapping$Data < 0.5, "between 40% and 50%",
                                                   ifelse(0.5 <= mapping$Data & mapping$Data < 0.6, "between 50% and 60%",
                                                          ifelse(0.6 <= mapping$Data & mapping$Data < 0.7, "between 60% and 70%",
                                                                 ifelse(0.7 <= mapping$Data & mapping$Data < 0.8, "between 70% and 80%",
                                                                        ifelse(0.8 <= mapping$Data & mapping$Data < 0.9, "between 80% and 90%",
                                                                               ifelse(0.9 <= mapping$Data & mapping$Data < 1, "above 90%",NA))))))))))
                                                                                 



# Maps


setwd("G:/My Drive/2019/1- FGM/11- Ad hoc tasks/ARHR supplement")

plot <- ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  coord_equal()+
  facet_wrap( Year ~.)+
  scale_fill_gradient(name = "FGM prevalence \n Women 15-19, %", low = "green3", high = "firebrick3",
                      labels = scales::percent_format(accuracy = 1L),  limits= c(0.20,1)) +
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = 2, barheight = 8))

ggsave(file = paste("Ethiopia.jpg"), print(plot))


# Map Nairobi -------------------------------------------------------------

setwd("G:/My Drive/2019/15- GlobalShapefiles/2018/UNmap_25")
geo <- readOGR(".", "BNDA25_CTY")

subset <- geo[(geo@data$ROMNAM=="Jammu and Kashmir"),]

subset2 <- geo[(geo@data$ROMNAM=="China"),]

#Correct country names
geo@data$ROMNAM <- as.character(geo@data$ROMNAM)
geo<- geo[!is.na(geo@data$ROMNAM) ,]
geo[geo@data$ROMNAM == "CÃ´te d'Ivoire", "ROMNAM"] <- "Cote d`Ivoire"
geo[geo@data$ROMNAM == "United Kingdom of Great Britain and Northern Ireland", "ROMNAM"] <- "United Kingdom"
names <- as.data.frame(unique(geo@data$ROMNAM))

geo_data <- tidy(geo)%>% #Command takes medium amount of time
  tbl_df()

subset_data <- tidy(subset) %>%
  tbl_df()

subset_data2 <- tidy(subset2) %>%
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- geo@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ROMNAM)%>%
  mutate(ROMNAM = as.character(ROMNAM))

geo_data_1$id <- as.numeric(geo_data_1$id)-1

geo_data_1$id <- as.character(geo_data_1$id)

subset_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ROMNAM)%>%
  mutate(ROMNAM = as.character(ROMNAM))

subset_data_1$id <- as.numeric(subset_data_1$id)-1

subset_data_1$id <- as.character(subset_data_1$id)

subset_data_12 <- subset2@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ROMNAM)%>%
  mutate(ROMNAM = as.character(ROMNAM))

subset_data_12$id <- as.numeric(subset_data_12$id)-1

subset_data_12$id <- as.character(subset_data_12$id)


# Read in data
setwd("G:/My Drive/2019/14- Nairobi/Harmful practice session")

data <-read.xlsx("data_FGM.xlsx", sheet = 1)

data_match <- data%>%
  tbl_df()

colnames(data_match) <- c("ROMNAM","Data")

#First link df2 (data of interest) and df3 (linking data)
data_match <- left_join(data_match, geo_data_1,by="ROMNAM")
#data_match2 <- left_join(data_match, subset_data_1,by="ROMNAM") 
#data_match2 <- data_match2[1:3]
#names(data_match2) <- c("ROMNAM", "Data", "id")
#data_match3 <- left_join(data_match, subset_data_12,by="ROMNAM") 
#data_match3 <- data_match3[1:3]
#names(data_match3) <- c("ROMNAM", "Data", "id")


data_match <- data_match %>%
  group_by(id)

#data_match2 <- data_match2 %>%
#  group_by(id)

#data_match3 <- data_match3 %>%
#  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(geo_data,data_match)
mapping2 <- left_join(subset_data,data_match2)
mapping3 <- left_join(subset_data2 ,data_match)

mapping$Data <- factor(mapping$Data, levels =c("FGM has been reported, no survey*", 
                                               "less than 10%", 
                                               "between 10% and 20%", "between 20% and 30%",
                                               "between 30% and 40%", "between 40% and 50%", 
                                               "between 60% and 70%", "between 70% and 80%", 
                                               "between 80% and 90%", "above 90%")) 

colfunc <- colorRampPalette(c("yellow", "red"))

png("G:/My Drive/2019/1- FGM/14- Map depository/3- FGM reported/FGMreported.with.legend2.png", 
    width = 25, height = 10, units = 'in', res = 900)

map1 <- ggplot()+
  geom_polygon(data=mapping, mapping = aes(x=long, y=lat, group=group, fill=Data), 
               color = "white", lwd = 0.5)+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_manual(values=c("deepskyblue1", colfunc(9)), na.value = "azure3")+
  theme_void()+
  theme(legend.position = c(0.12,0.3), legend.text = element_text(size = 16),legend.key.size = unit(1, "cm"),
        plot.background = element_rect(fill = "slategray1"))+
  labs(fill = " ")

map1 + geom_polygon(data=mapping2,mapping = aes(x=long, y=lat), 
                            color = "azure3", linetype = "dashed", fill=NA, lwd = 0.5)


dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/3- FGM reported/FGMreported.png", width = 12, height = 7, units = 'in', res = 600)

ggplot()+
  geom_polygon(data=mapping, mapping = aes(x=long, y=lat, group=group, fill=Data))+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_manual(values=c("deepskyblue1", colfunc(9)), na.value = "azure3")+
  borders("world", col = "white", lwd = 0.5)+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "slategray1"))+
  labs(fill = " ")

dev.off()

# map for Data Signature session

mapping2 <- na_if(mapping, "FGM has been reported but no household survey available")

png("G:/My Drive/2019/1- FGM/14- Map depository/2- National map/national.map.with.legend.png", 
    width = 12, height = 7, units = 'in', res = 600)

ggplot()+
  geom_polygon(data=mapping2, mapping = aes(x=long, y=lat, group=group, fill=Data))+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_manual(values=c(colfunc(9)), na.value = "azure3")+
  borders("world", col = "white", lwd = 0.5)+
  theme_void()+
  theme(legend.position = c(0.10,0.2),
        plot.background = element_rect(fill = "slategray1"))+
  
  labs(fill = " ")
dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/2- National map/national.map.png", width = 12, height = 7, units = 'in', res = 600)

ggplot()+
  geom_polygon(data=mapping2, mapping = aes(x=long, y=lat, group=group, fill=Data))+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_manual(values=c(colfunc(9)), na.value = "azure3")+
  borders("world", col = "white", lwd = 0.5)+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "slategray1"))+
  labs(fill = " ")

dev.off()

png("G:/My Drive/2019/1- FGM/14- Map depository/2- National map/national.map.simple.png", width = 12, height = 7, units = 'in', res = 600)

ggplot()+
  geom_polygon(data=mapping2, mapping = aes(x=long, y=lat, group=group, fill=Data))+
  coord_equal(ylim = c(-55,80), xlim = c(-150, 165))+
  scale_fill_manual(values=c(colfunc(9)), na.value = "azure3")+
  #borders("world", col = "white", lwd = 0.5)+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"))+
  labs(fill = " ")

dev.off()


# Map Kenya ---------------------------------------

setwd("G:/My Drive/2019/14- Nairobi")

results <-read.csv("Kenya.csv") # load subnational prevalence rates for fgm women 15-49
geo <- readOGR(".", "sdr_subnational_boundaries") # load shapefile for  DHS data

subset <- geo
subset@data$ID <- paste(subset@data$CNTRYNAMEE, subset@data$DHSREGEN, sep="" )
subset@data$ID2 <- paste(subset@data$CNTRYNAMEE)

unique(subset@data$DHSREGEN)

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID, ID2)%>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID2 = as.character(ID2))

geo_data_1$id <- as.numeric(geo_data_1$id)-1

geo_data_1$id <- as.character(geo_data_1$id)

data <- results %>%
  dplyr::select(c("ID", "data", "year"))

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID", "Data", "Year") 

data_match <- left_join(data_match, geo_data_1,by=c("ID"="ID"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

mapping$Data <- as.numeric(mapping$Data)

mapping$Fill <- ifelse(mapping$Data < 0.1, "less than 10%", 
                       ifelse(0.1 <= mapping$Data & mapping$Data < 0.2, "between 10% and 20%",
                              ifelse(0.2 <= mapping$Data & mapping$Data < 0.3, "between 20% and 30%",
                                     ifelse(0.3 <= mapping$Data & mapping$Data < 0.4, "between 30% and 40%",
                                            ifelse(0.4 <= mapping$Data & mapping$Data < 0.5, "between 40% and 50%",
                                                   ifelse(0.5 <= mapping$Data & mapping$Data < 0.6, "between 50% and 60%",
                                                          ifelse(0.6 <= mapping$Data & mapping$Data < 0.7, "between 60% and 70%", 
                                                                 ifelse(0.7 <= mapping$Data & mapping$Data < 0.8, "between 70% and 80%",
                                                                        ifelse(0.8 <= mapping$Data & mapping$Data < 0.9, "between 80% and 90%",
                                                                                ifelse(0.9 <= mapping$Data, "higher than 90%", NA))))))))))


mapping$Fill <- as.factor(mapping$Fill)

unique(mapping$Fill)

mapping$Fill = factor(mapping$Fill,levels(mapping$Fill)[c(6,1,2,3,4,5)]) 

# Maps

png("G:/My Drive/2019/1- FGM/14- Map depository/1- Subnational map/kenya.png", width = 10, height = 5, units = 'in', res = 600)

ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Fill))+
  geom_polygon()+
  theme_void()+
  coord_equal()+
  facet_wrap( Year ~.)+
  scale_fill_brewer(palette='YlOrRd')+
  theme(legend.position = "none", strip.text.x = element_blank())+
  labs(fill = " ")

dev.off()
