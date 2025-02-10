#### Work flow ####

####Complied by Kagiso Nhlapo 
#### MSC student Working on Hydrologic Refugia spatial characteristics 
#### Last edited 24 february 2022

####   To do  ####
###1) Get libraries etc
###2) remove geometry from the the capeFR_for_heatmap
### (y should not have class sf; for spatial joins)
###3) use Left_join function to retains all rows of the data on the Left side (ObNRE).
###3) join the QDS count data with QDS layer


#### Libraries ####
library(tidyverse)
library(sf)
library(terra)

if(Sys.getenv("USER") == "jasper") {
  bigdat <- "/Users/jasper/Dropbox/UCT/Students/KagisoNhlapo/BigDataShare"}

#Kagiso sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "") {
  bigdat <- "/Users/NHLKAG001/Dropbox/BigDataShare"}


#loading files saved from Script 1
CapeFR_for_heatmap <- readRDS(file = "Data/CapeFR_for_heatmap")
NREs <- readRDS(file = "Data/NREs")
qds <- readRDS("Data/qds")
FctNRE <- readRDS("Data/FctNRE")
ObNRE <- readRDS("Data/obNRE")
Dom.geo <-readRDS("Data/Dom.geo")

#dissolved QDS

#dissolved_qds <- qds %>%
  #group_by(qdgc) %>%
  #summarise(pentad = first(pentad)) %>%
  #left_join(qds, by = "QDS") %>%
  #st_cast("MULTIPOLYGON")  # Cast the geometry to multipolygon (optional, depending on your data)



#remove geometry from the the capeFR_for_heatmap
CapeFR_for_heatmap_ObNRE <- CapeFR_for_heatmap%>% st_set_geometry(NULL)

#returns all records from the left table (table1), and the matching records from the right table (table2)
ObNRE_heatmap <- left_join(ObNRE, CapeFR_for_heatmap_ObNRE, by = "Taxon") 

#return only Taxon, QDS, QDScount columns
ObNRE_heatmap <- ObNRE_heatmap %>% dplyr::select("Taxon", "QDS", "QDScount","geometry")

#To find Narrow range endemics, filter only QDSs ,<4
ObNRE_heatmap <- ObNRE_heatmap %>% filter(QDScount < 5)


#the count of species present per QDS with no duplicates
Richness.ObNRE <- ObNRE_heatmap %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  dplyr::select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(species_richness = n()) #Count of the number of species present per QDS 

#Joining the two data sets and omiting nas
Richness.ObNRE.QDS <- left_join(qds, Richness.ObNRE, by = c("Name"= "QDS"))

#The crop the sf object using st_crop then remove all the NAs from the data 

Richness.ObNRE.QDS <- st_crop(Richness.ObNRE.QDS, Dom.geo) %>% filter(!is.na(species_richness))

Richness.ObNRE.QDS <- Richness.ObNRE.QDS %>% 
  mutate(species_richness = coalesce(species_richness, 0))

#Important plot to visualize the data 
ggplot()+
  geom_sf(data = Richness.ObNRE.QDS, aes(fill = species_richness))+
  geom_polygon() + theme(panel.background = element_rect(fill = "white"))

########Facultative #######

CapeFR_for_heatmap <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#returns all records from the left table (table1), and the matching records from the right table (table2)
FctNRE_heatmap <- left_join(FctNRE, CapeFR_for_heatmap, by = "Taxon") 

#return only Taxon, QDS, QDScount columns
FctNRE_heatmap <- FctNRE_heatmap %>% dplyr::select("Taxon", "QDS", "QDScount","geometry")

#To find Narrow range endemics, filter only QDSs ,<4
FctNRE_heatmap <- FctNRE_heatmap %>% filter(QDScount < 5)


#the count of species present per QDS with no duplicates
Richness.FCTNRE <- FctNRE_heatmap %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  dplyr::select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(species_richness = n()) #Count of the number of species present per QDS 

#Joining the two data sets and omiting nas
Richness.FCTNRE.QDS <- left_join(qds, Richness.FCTNRE, by = c("Name"= "QDS"))

#The crop the sf object using st_crop then remove all the NAs from the data 
Richness.FCTNRE.QDS <- st_crop(Richness.FCTNRE.QDS, Dom.geo) %>% filter(!is.na(species_richness))

Richness.FCTNRE.QDS <- Richness.FCTNRE.QDS %>% 
  mutate(species_richness = coalesce(species_richness, 0))

#Important plot to visualize the data 
ggplot()+
  geom_sf(data = Richness.FCTNRE.QDS, aes(fill = species_richness))+
  geom_polygon()+ theme(panel.background = element_rect(fill = "white"))

#save so objects in directory

saveRDS(Richness.FCTNRE.QDS, file = "Data/Richness.FCTNRE.QDS")
saveRDS(Richness.ObNRE.QDS, file = "Data/Richness.ObNRE.QDS")

#remove objects not needed
rm(Dom.geo, FctNRE, FctNRE_heatmap,NREs, ObNRE, ObNRE_heatmap, 
   qds, Richness.FCTNRE, Richness.FCTNRE.QDS, Richness.ObNRE, Richness.ObNRE.QDS,
   bigdat, CapeFR_for_heatmap, CapeFR_for_heatmap_ObNRE)

###################### Proceed to script five (5, Adding response variables to the data frame) 




