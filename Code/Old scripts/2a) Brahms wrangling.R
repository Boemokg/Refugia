#### Work flow ####

####Complied by Kagiso Nhlapo 
#### MSC student Working on Hydrologic Refugia spatial characteristics 
#### Last edited 25 November 2022

####   To do  ####
###1) Get libraries etc
###2) Read in locality data (Brahms)
###3) Remove all NAs from the Lontitude and latitude, the QDS as well as LatLongAccuracy column
###4) Change LatLongAccuracy from class character to class factor and keep localities with good acurracy
###5) Assign Brahms CRS
###6) Read in the Fynbos Biome Mask/shape file and prep for analysis
###7) Set a CRS for the Mask
###8) Filter/intersect points with Fynbos Biome Mask
###9) Filter for species QDS <= 4 (Narrow endemics at a fine scale)
###10) Extract full Brahms data for species with QDS <= 4

####   Get packages from libraries ####
#library(rgdal)
library(tidyverse)
library(readxl)
library(readr)
library(sf)
library(units)
library(terra)
library(mapview)
library(viridis)
#### Data cleaning Brahms ####

#Read SANBI data from http://newposa.sanbi.org/sanbi/Explore
brahms <- read_excel("Big Data/2022-08-11_064413137-BRAHMSOnlineData.xlsx")

#Remove all NAs from the Lontitude and latitude columns as well as the QDS column
brahms <- brahms %>% filter(!is.na(Longitude)) %>% filter(!is.na(QDS))

#Removing Nas from Latlongaccuracy 
brahms <- brahms %>% filter(!is.na(LatLongAccuracy))

#converting LatLongAccuracy from character to factor variable
#brahms$LatLongAccuracy = as.factor(brahms$LatLongAccuracy)

#keep all accurate localities by filtering and keeping those with accuracy less than or = 10k and within a 1/4dg
brahms <- brahms %>% filter(LatLongAccuracy %in% c("100m", "10k", "1k ", "50m", "250m","5k",
                                                   "10m","2k", "1000m","0.25k", "5m", "1/4g",
                                                   "1/4","¼dg", "04/01", "1/4dg"))


#assign coordinate reference system to the new brahms data for plotting
brahms <- st_as_sf(brahms, coords = c("Longitude", "Latitude"), crs = 4326)

#Here we are going to run the fynbos biome code to get the fynbos & Veg mask 

### Get veg types, trim extra columns and remove self-intersections
fynbos <- st_read("Data/fynbos.gpkg") %>%
  dplyr::select(Name_18, BIOREGION_) %>%
  st_make_valid()

### Select only "Fynbos" vegetation types (i.e. drop Renosterveld or Strandveld) and dissolve into one big polygon
fynveg <- fynbos %>%
  filter(str_detect(Name_18, "Fynbos")) %>% # Selects only Veg Type names that incluse "Fynbos"
  summarize() #Dissolves polygons 

#transform the fynbos veg mask coordinate reference system 
fynveg <- st_transform(fynveg, crs = 4326)

#include only species that are found only within fynbos
CapeFR <- sf :: st_intersection(brahms, fynveg)


#create a CapeFR for heat map where we look at the species count per QDS, so that its easier to plot
CapeFR_for_heatmap <- CapeFR


#Summarize Brahms data to count number of QDS per species
CapeFR <- CapeFR %>% 
  group_by(Taxon) %>% 
  summarize(QDScount = length(unique(QDS, na.rm = T)))

  #### SA Plant list data ####

#1) Load SA spp list
#2) From The plant list filter alien invasive plants 
#3) from these data use anti_join() to get species unique to CapeFR
#4) These are new species without alien invasive plants 

#load South African National Plant Checklist
SAspp <- read_excel("Data/SA-Plant-Checklist-2022.xlsx")

#filter South African National Plant Checklist for alien invasives 
Alienspp <- SAspp %>% filter(`South Africa` %in% c("notindig; nat","notindig; cult; nat; inv",
                                                "notindig; cult; nat","notindig; nat; inv","notindig",
                                                "cryptogenic","notindig; nat; inv; eradicated","notindig; cult",                               
                                                 "unconf", "notindig; unconf", "NA"))

#Create an extra column by only including the genus and sp name 
Alienspp <- Alienspp %>% mutate(Taxon = paste(Genus, SP1) %>% trimws())

#Here we are find species unique to Brahm/Capefr (i.e without alien invasives)
CapeFR <- dplyr::anti_join (CapeFR, Alienspp, by = "Taxon")


#### Data cleaning TSP ####
#1) load the Red-list/tspp data
#2) set a CRS for the TSP data
#3) filter species using the Fynbos mask (keeping species restricted  to just fynbos)
#4) Extract species unique to tspp (i.e. not in SApp)
#5) From these data paste the genius and Species name together to get full species name without the author 

#get threatened species data and filter by data quality
tspp <- read_delim("Data/WC_TOCC.txt", delim = "\t")

#assign the threatned species data a coordinate reference system
tspp <- st_as_sf(tspp, coords = c("Long", "Lat"), crs = 4326) 

#Overlay with QDS mask to only include Species found only in fynbos region 
tspp <- sf :: st_intersection(tspp, fynveg)

#Since Saspp & Tspp have species name written in the same format, here we obtain species unique to SAspp
newspp <- dplyr::anti_join(SAspp, tspp, by = "Taxon")
#newspp <- dplyr::left_join(SAspp, tspp, by = "Taxon")

#These species names are then built by merging/mututating the genius and species name
newspp <- newspp %>% mutate(Taxon = paste(Genus, SP1) %>% trimws())

#### Narrow Range Endemics ####

#Return all rows from capeFR where there are matching names in newspecies,
NREs <- dplyr::inner_join(CapeFR, newspp, by = "Taxon")


#Finding duplicates on the NRE species list we have 
duplicated_taxa <- NREs %>%
  group_by(Taxon) %>%
  summarise(n_rows = n()) %>%
  filter(n_rows > 1) %>%
  arrange(desc(n_rows)) %>%
  pull(Taxon)

#the duplicated taxa characters
duplicated_taxa

#removing duplicate taxa from the data (Validation step)
NREs <- NREs %>%
  filter(!(Taxon %in% duplicated_taxa))

#To find Narrow range endemics, filter only QDSs ,<4
NREs <- NREs %>% filter(QDScount < 5)

#Removing unwanted colums 
#NREs <- NREs[1:3346,1:3]
NREs <- NREs %>% select(Taxon:geometry)
#Getting 

#removing turning mutltipoint to point
NREs = st_cast(NREs, "POINT", group_or_split = TRUE )

#checking if all point are indeed point and not multipoint
st_geometry_type(NREs)
