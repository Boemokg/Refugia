
#### Cleaning and Collating ####

#Complied by Kagiso Nhlapo 
#MSc student Working on Hydrologic Refugia spatial characteristics 
#Last edited 17 October 2022

####  To do  ####
###1) Get libraries etc
###2) Read in Brahms data & TSPP data 
###3) Organize Brahms data and calculate QDS counts
###4) Filter for species QDS <= 4 (Narrow endemics at a fine scale)
###5) Extract full Brahms data for species with QDS <= 4
###6) Extract species unique to Brahms (i.e. not in TSPP)
###7) Remove all Brahms rows with missing/NA lat-long and QDS values 
### Add: From unique Brahms data add those with good location accuracy to Tspp data 
###8) Plot to look at possible points
###9) Read in the Fynbos Biome Mask/shape file and prep for analysis
###10) Filter/intersect points with Fynbos Biome Mask
###11) Get points that don't intersect and Brahms data with poor location accuracy
### Manual filtering fromo step 11... to be decided...

####   Get packages from libraries ####
library(rgdal)
library(tidyverse)
library(readxl)
library(ggpubr)
library(sf)
library(terra)
library(mapview)
library(raster)
library(sp)

#### Data cleaning Bramahs and Tspp data ####

#Read SANBI data from http://newposa.sanbi.org/sanbi/Explore
brahms <- read_excel("~/Big data/2022-08-11_064413137-BRAHMSOnlineData.xlsx") 
View(Brahms)

#get threatened species data and filter by data quality
tspp <- read_delim("WC_TOCC.txt", delim = "\t")

#assign the threatned species data a coordinate reference system
tspp <- st_as_sf(tspp, coords = c("Long", "Lat"), crs = 4326) 

####   Filter By QDS ####
#Summarize Brahms data to count number of QDS per species
sppQDS <- brahms %>% group_by(Taxon) %>% summarize(QDScount = length(unique(QDS, na.rm = T)))


#Filtering QDS <= 4,5 and 6 too
#filter for <5 to find narrow range endemics 
sppQDS4 <- sppQDS %>% filter(QDScount < 5)


# Extracted full Brahms data for species with <4 QDS records
sppQDSdat <- brahms %>% filter(Taxon %in% sppQDS$Taxon)


### Extract species unique to Brahms but not in tspp
newsppQDS <- anti_join(brahms, tspp, by = c("Taxon"))
attach(newsppQDS)

#Remove rows with "NA' in the longitude colunm
newsppQDS= brahms %>% filter(!is.na(Longitude)) %>% filter(!is.na(QDS))

#assign coordinate reference system to the new brahms data for plotting
newplot <- st_as_sf(newsppQDS, coords = c("Longitude", "Latitude"), crs = 4326) 

#Extracting the full brahms data set
<<<<<<< HEAD
newplot <- CapeFR %>% group_by(Taxon) %>% summarize(QDScount = length(unique(QDS, na.rm = T)))


#Here we are trying to just get localitites in the 12 bioregions (Excluding Renosterveld or Strandveld bioregions)
#Run the fynbos script then filter/intersect points with Fynbos Biome Mask.

#transform the fynbos mask coordinate reference system 
fynbos <- st_transform(fynbos, crs = 4326)

#transform the fynbos veg mask coordinate reference system 
fynveg <- st_transform(fynveg, crs = 4326)

#include only species that are found only within fynbos
CapeFR <- sf :: st_intersection(brahms, fynveg)


CapeFR <- CapeFR %>% filter(QDScount < 5)

=======
newplot <- newplot %>% group_by(Taxon) %>% summarize(QDScount = length(unique(QDS, na.rm = T)))


#Here we are trying to just get localitites in the 12 bioregions (Excluding Renosterveld or Strandveld bioregions)
#Run the fynbos script then filter/intersect points with Fynbos Biome Mask.

fynbos <- st_transform(fynbos, crs = 4326)


CapeFR <- sf :: st_intersection(newplot, fynbos)

>>>>>>> 02b2ca8a82d7b56146be5373c7d82ddcd8d0f53b
#visualising the plot with mapview 
mapView(x = CapeFR, legend = F, label = "Taxon")

#visualising with ggplot

ggplot() +
  geom_sf(data = CapeFR, aes(fill = QDScount))

###Filter points by quality () ####

#Remove Nas from LatLongAccuracy
brahms <- brahms %>% filter(!is.na(LatLongAccuracy))


#Remove Nas from Lat-Long and QDS columns 
brahms <- brahms %>% filter(!is.na(Longitude)) %>% filter(!is.na(QDS))

#Convert LatLongAccuracy to a factor 
brahms$LatLongAccuracy = as.factor(brahms$LatLongAccuracy)

#Filter LatLongAccuracy by quality
brahms <- brahms %>% filter(LatLongAccuracy %in% c("1/4dg ","100m", "10k", "1k ", "50m", "250m","5k","10m",
                                                   "1dg","100m", "2k", "1000m","0.25k", "1/2dg", "5m", "1/4g",
                                                   "1/4g","1/4","¼dg", "04/01"))







