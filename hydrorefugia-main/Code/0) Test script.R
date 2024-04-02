#workflow
#created by kagiso nhlapo 
#msc hydrologic refugia 


#1) get libraries and set user specific working directories etc
#2) read the the vector data and the shape file 
#3) crop the global shapefile with the fynveg shapefile 
#4) From there find the propotion of species in each QDS 
#5) plot the data 

#Jasper sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "jasper") {
  bigdat <- "/Users/jasper/Dropbox/UCT/Students/KagisoNhlapo/BigDataShare"}

#Kagiso sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "") {
  bigdat <- "/Users/user/Dropbox/BigDataShare"}

#Load saved files from Data
Richeness.NRE.QDS <- readRDS(file = "Data/Richeness.NRE.QDS.RDS")
Richness.ObNRE.QDS <- readRDS(file = "Data/Richness.ObNRE.QDS.RDS")
Richness.FCTNRE.QDS <- readRDS(file = "Data/Richness.FCTNRE.QDS.RDS")



library(tidyverse)
library(terra)
library(sf)
library(rgdal)

#Read the Vegan data 
Vegan_data_full <- read_csv(paste0(bigdat,"/Vegan_data_full.csv"))

#Read the Global QDS shapefile  
Fullspp <- st_read(paste0(bigdat,"/Global_QDG.shp"))

### data wrangling ####
dat <- Vegan_data_full

#count the number of species number by QSDS
dat$SR = rowSums(dat[,-c(1,2)]>0)

#Only keep 3 columns "...1" , "QDgrid", "SR"
dat <- dat %>% dplyr:: select("...1" , "QDgrid", "SR")

#Upper case the QDS column 
dat$QDgrid <- toupper(dat$QDgrid)

#Upper case the QDS column 
Fullspp$qdgc <- toupper(Fullspp$qdgc)

#Remove edit QDS string 
Fullspp$QDS <-str_sub(Fullspp$qdgc, start = 3, end = 9)
 
Fullspp$QDS <-str_replace(Fullspp$QDS,"[NS]","")

#returns all records from the left table (table1), and the matching records from the right table (table2)
datGlobal <- left_join(dat, Fullspp, by = c("QDgrid" = "QDS"))

####################################################################
### Plotting NARROW RANGE ENDEMIC PROPORTIONS #####
####################################################################

#match the datglobal qds to the NRES
Fullspp = right_join(Richeness.NRE.QDS, datGlobal, by = c("qdgc" = "QDgrid")) %>%
  dplyr:: select(qdgc,species_richness,SR)

#creating the species richness propotions 
Fullspp$spprop <- Fullspp$species_richness/Fullspp$SR

#Creating the heatmap of proportions
ggplot()+
  geom_sf(data = Fullspp, aes(fill = spprop))+
  geom_polygon()
 

####################################################################
### Plotting Obligate wetland NARROW RANGE ENDEMIC PROPORTIONS #####
####################################################################

#match the datglobal qds to the Obligate NRES 
Fullspp_obnre = right_join(Richness.ObNRE.QDS, datGlobal, by = c("qdgc" = "QDgrid")) %>%
  dplyr:: select(qdgc,species_richness,SR)

#creating the species richness proportions 
Fullspp_obnre$spprop <- Fullspp_obnre$species_richness/Fullspp_obnre$SR#Creating the heatmap of propotions

#Creating the heatmap of proportions
ggplot()+
  geom_sf(data = Fullspp_obnre, aes(fill = spprop))+
  geom_polygon()

####################################################################
### Plotting Facultative wetland NARROW RANGE ENDEMIC PROPORTIONS #####
####################################################################

Fullspp_fctnre = right_join(Richness.FCTNRE.QDS, datGlobal, by = c("qdgc" = "QDgrid")) %>%
  dplyr:: select(qdgc,species_richness,SR)

#creating the species richness proportions 
Fullspp_fctnre$spprop <- Fullspp_fctnre$species_richness/Fullspp_fctnre$SR

#Creating the heatmap of proportions
ggplot()+
  geom_sf(data = Fullspp_fctnre, aes(fill = spprop))+
  geom_polygon()


