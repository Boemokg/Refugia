##############################################################################
######## Code for heat map
##############################################################################
######## Compiled by kagiso Nhlapo
######## Last edited: 05 December 2022
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
#library(ggpubr)
#library(MASS)
#library(ggplot2)
library(viridis)
library(stars)



#### read QDS Layer ####

#read the QDS layer 
qds <- st_read("Data/qds_pentads.geojson") %>% filter(country %in% c("South Africa"))


####joining the QDS count data to the QDS layer.####

#Changing the column name the "qdgc" so we can join the two data sets by "qdgc"
#colnames(CapeFR_for_heatmap)[colnames(CapeFR_for_heatmap)=="QSD"] <- "qdgc"

#Join using st_join
#Full_CapeFR <- st_join(CapeFR_for_heatmap, qds, by = "qdgc")

#then find the qds count of all the species including the QDS column 
#Full_CapeFR <- Full_CapeFR %>% group_by(Taxon) %>% summarize(QDScount = length(unique(qdgc, na.rm = T)))

####Species count per QDS ####

#species count
#spp_count = NREs %>% group_by(QDScount) %>% summarise(species_richness = n())

#the count of species present per QDS with no duplicates
hmm <- CapeFR_for_heatmap %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(species_richness = n()) #Count of the number of species present per QDS 

#remove the pentads present here so that we only deal with QDS counts 
QDS <- qds %>% group_by(qdgc) %>% summarise()

#Joining the two data sets and omiting nas
hmm.QDS <- left_join(QDS, hmm, by = c("qdgc"= "QDS")) %>%
  na.omit()
#Important plot to visualize the data 
ggplot()+
  geom_sf(data = hmm.QDS, aes(fill = species_richness))+
  geom_polygon()
  
#Heat_count <- CapeFR_for_heatmap %>% group_by(QDS, Taxon) %>% summarise(counts = n())

 
#duplicated_taxa_Map <- Heat_count %>%
  #summarise(n_rows = n()) %>%
  #filter(n_rows > 1) %>%
  #arrange(desc(n_rows))

#removing the duplicates 
#NREs <- Heat_count %>%
  #filter(!(QDS %in% duplicated_taxa_Map))

#visualize ?
#ggplot()+ 
  #geom_sf(data = Spp_count, aes(colour = species_richness))

#Extracting lat and long coordinates from the geomtery column in the Nres Object 
#NREs_for_heatmap <- NREs %>% tidyr::extract(geometry, c('lat', 'long'), '\\((.*), (.*)\\)', convert = TRUE) 
#rm(NREs_for_heatmap$geometry)

#removing geometry columns
#NREs_for_heatmap <- select(NREs_for_heatmap,-3, -6)


#plot heat map with the qds layer 
#ggplot() + 
  #geom_sf(data = NREs, aes(colour = QDScount))+ #narrow endemic data
  #geom_sf(data = qds) #QDS mask


#Join QDS layer with NRE endemic species 

#joing the qds layers and NREs
#NRE.QDS <- st_join(qds, NREs)

#removed ROWS without QDS COUNTS 
#NRE.QDS <- NRE.QDS %>% filter(!is.na(QDScount))

#plot map
#ggplot()+
  #geom_sf(data = NRE.QDS, aes(fill = QDScount))


#Removing points that are not in the CFR







