##### Creating Spatial heat map ####

# Second Brahms script
# Complied by Kagiso Nhlapo 
#  MSc student at UCT work on data analysis Hydrologic refugia 
# Last edited 17 October 2022

library(tidyverse)
library(ggpubr)
library(units)
library(sf)
library(terra)
library(raster)
library(sp)
library(ggplot2)




####  To do  ####
###1) Get libraries etc
###2) Read Brahms script 2
###3) Read in the wetland data 
###4) Filter using wetland species 
###5) Filter by fynbos biome essentially creating bioregions 
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
###6) Extract lat and long from the filtred data 
###7) Creating a column with unique QDS counts 
###8) From this create a plot using the using this QDS count table 

##### Get Libraries ####

library(readxl)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(tidyverse)

#### NRE ####


### Get veg types, trim extra columns and remove self-intersections
fynbos <- st_read("fynbos.gpkg") %>%
  dplyr::select(Name_18, BIOREGION_) %>%
  st_make_valid()

### Select only "Fynbos" vegetation types (i.e. drop Renosterveld or Strandveld) and dissolve into one big polygon
fynveg <- fynbos %>%
  filter(str_detect(Name_18, "Fynbos")) %>% # Selects only Veg Type names that incluse "Fynbos"
  summarize() #Dissolves polygons 

ggplot() +
  geom_sf(data = fynveg)

#Select only "Fynbos" bioregions (i.e. drop Renosterveld or Strandveld)

fynbos <- filter(fynbos, BIOREGION_ %in% c("South Coast Fynbos Bioregion ", "Northwest Fynbos Bioregion","Southern Fynbos Bioregion ",
                                           "Namaqualand Cape Shrublands Bioregion","Southwest Fynbos Bioregion"))

#Cleaning fynbos bioregions
bioregions <-fynbos %>% filter(str_detect(BIOREGION_, "Fynbos")) %>%
  mutate(Bioregion = fct_drop(BIOREGION_)) %>%
  group_by(Bioregion) %>% summarize() %>%
  mutate(Bioregion = as.character(Bioregion))
# Plotting bioregions 
ggplot() + geom_sf(data = bioregions, aes(fill = Bioregion, color = Bioregion)) +  
  colour = as.numeric(as.factor(Name_18)) +
  scale_fill_viridis(discrete = T) + cale_color_viridis(discrete = T) +
  geom_sf_text(data = stas, aes(label =shortname)) # Extra code if you want to plot text


#Read in wetland data from the Sieben et al study 
Wetspp <- read_excel("Obligate_wetland_species.xlsx") 
#Attach the data to better call the columns 
attach(Wetspp)

#Filter the wetland species by calling only wetland dependent species
Wetspp <- Wetspp %>% filter(WetlandDependence...3 == 1)

Wetspp

Wetspp = anti_join(Wetspp, newsppQDS, by = c("Taxon"))




####  Wetland shape file ####
wetland <- sf::st_read("wriall500.shp")
wetland %>% mapview()