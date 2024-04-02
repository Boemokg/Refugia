##############################################################################
######## Code for heat map
##############################################################################
######## Compiled by kagiso Nhlapo
######## Last edited: 22 December 2022
##############################################################################
##############################################################################
###Steps:
###1) Get libraries etc
###2) Read in locality data and clean from script 2a
###3) Read the QDS layer 
###4) count the number of species in each QDS a plot that 
###5) or join the QDS count data with QDS layer

#### Libraries ####
library(tidyverse)
library(sf)
library(terra)
library(MASS)
library(viridis)
library(stars)

#Jasper sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "jasper") {
  bigdat <- "/Users/jasper/Dropbox/UCT/Students/KagisoNhlapo/BigDataShare"}

#Kagiso sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "") {
  bigdat <- "/Users/NHLKAG001/Dropbox/BigDataShare"}


#loading files saved from Script 1
CapeFR_for_heatmap <- readRDS(file = "Data/CapeFR_for_heatmap.rds")
NREs <- readRDS(file = "Data/NREs.RDS")

#### read QDS Layer ####

#read the QDS layer 
#if(!file.exists(paste0(bigdat,"/qds.shp"))){
  #qds <- st_read("https://api.birdmap.africa/sabap2/v2/pentads/project/sabap2?format=geoJSON")
  #st_write(qds, paste0(bigdat,"/qds.shp"))
#} else {
  #qds <- st_read(paste0(bigdat,"/qds.shp"))
#}
#dissolved_qds <- qds %>%
  #group_by(qdgc) %>%
  #summarise(pentad = first(pentad)) %>%
  #left_join(qds, by = "QDS") %>%
  #st_cast("MULTIPOLYGON") 
#Keep only Western cape QDS grids 
#qds = qds %>% 
#filter(province %in% c("Western Cape", "Northern Cape","Eastern Cape"))

qds <- st_read("C:/Users/NHLKAG001/Downloads/QDS_SA.kml")

#remove geometry from the the capeFR_for_heatmap
CapeFR_for_heatmap_NREs <- CapeFR_for_heatmap %>% st_set_geometry(NULL)


#returns all records from the left table (table1), and the matching records from the right table (table2)
NRE_heatmap <- left_join(NREs, CapeFR_for_heatmap_NREs, by = "Taxon") 


#Select only Taxon, QDS, QDScount and geometry columns
NRE_heatmap <- NRE_heatmap %>% dplyr:: select("Taxon", "QDS", "QDScount","geometry")

#Recalculating the QDS
NRE_heatmap <- NRE_heatmap %>% filter(QDScount < 5)


#the count of species present per QDS with no duplicates
Richeness.NRE <- NRE_heatmap %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  dplyr::select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(species_richness = n()) #Count of the number of species present per QDS 

#remove the pentads present here so that we only deal with QDS counts 
#QDS <- qds %>% group_by(qdgc) %>% summarise()

#Joining the two data sets and omiting nas
Richeness.NRE.QDS <- left_join(qds, Richeness.NRE, by = c("Name"= "QDS")) 

#Crop the richness per QDS object using the Dominate geology layer
#first run dom.geom 
# Dom.geo <-terra::rast(raster::raster(paste0(bigdat,"/lith_simplif.grd")))
# writeRaster(Dom.geo, paste0(bigdat,"/lith_simplif.tif"))
Dom.geo <-terra::rast(paste0(bigdat,"/lith_simplif.tif"))

#The crop the sf object using st_crop then remove all the NAs from the data 
Richeness.NRE.QDS <- st_crop(Richeness.NRE.QDS, Dom.geo) %>% filter(!is.na(species_richness))

#Important plot to visualize the data 
ggplot()+
  geom_sf(data = Richeness.NRE.QDS, aes(fill = species_richness, colour = species_richness))+
  geom_polygon() +theme(panel.background = element_rect(fill = "white"))



#save QDS layer
#saveRDS(dissolved_qds, file = "Data/dissolved_qds.RDS")
saveRDS(Richeness.NRE.QDS, file = "Data/Richeness.NRE.QDS")
saveRDS(Dom.geo, file = "Data/Dom.geo")
saveRDS(qds, file = "Data/qds")

#remove all data sets from the environment 
rm(CapeFR_for_heatmap, CapeFR_for_heatmap_NREs, Dom.geo, NREs, NRE_heatmap, qds, Richeness.NRE, Richeness.NRE.QDS, bigdat)

###################### Proceed to script three (3, wetland species Analysis heatmaps) 






