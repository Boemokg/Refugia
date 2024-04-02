#### Work flow ####

####Complied by Kagiso Nhlapo 
#### MSC student Working on Hydrologic Refugia spatial characteristics 
#### Scrpit 5 of 5 in sptial heatmap plots 
#### Last edited 11 April 2023

####   To do  ####
###1) Get libraries etc
###2) Read the 3 Raster files
###3) Extract their mean precipitation values from the raster
###4) Add these values on the Cape_For_heatmap data set
###4) The Left join NREs to get all matching Cape_For_Heat_map
###5) Get the species count of species in each QDS
###6) The create a heat map of mean annual in each month

#Jasper sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "jasper") {
  bigdat <- "/Users/jasper/Dropbox/UCT/Students/KagisoNhlapo/BigDataShare"}

#Kagiso sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "") {
  bigdat <- "/Users/NHLKAG001/Dropbox/BigDataShare"}

#loading files saved from Script 1
CapeFR_for_heatmap <- readRDS(file = "Data/CapeFR_for_heatmap.rds")
NREs <- readRDS(file = "Data/NREs.rds")
qds <- readRDS("Data/qds") %>% dplyr:: select(-Description)
Dom.geo <-terra::rast(paste0(bigdat,"/lith_simplif.tif"))
Richness.FCTNRE.QDS <- readRDS("Data/Richness.FCTNRE.QDS.RDS") %>% st_set_geometry(NULL) %>% 
  rename("Facultative_species" = "species_richness") %>% dplyr:: select(-Description)
Richness.ObNRE.QDS <- readRDS("Data/Richness.ObNRE.QDS.RDS")%>% st_set_geometry(NULL) %>%
  rename("Obligate_species" = "species_richness") %>% dplyr:: select(-Description)


#### Libraries ####
library(tidyverse)
library(sf)
library(terra)
library(stringr)

#read the raster rather so that we can extract the species richness values from there 
species_richness_QDS  <- read_csv("C:/Users/NHLKAG001/Downloads/doi_10_5061_dryad_8w9ghx3m8__v20210423/species-richness_QDS.csv")


#remove geometry from the the capeFR_for_heatmap
CapeFR_for_heatmap <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#returns all records from the left table (table1), and the matching records from the right table (table2)
NRE_heatmap_Cloud <- left_join(NREs, CapeFR_for_heatmap, by = "Taxon") 


#return only Taxon, QDS, QDScount columns
#NRE_heatmap_Cloud <- NRE_heatmap_Cloud %>% 
  #plyr:: select("Taxon", "QDS", "QDScount","geometry")

###### HEATMAP ####

Heat.Mapdat <- NRE_heatmap_Cloud %>% #original data from 
  st_set_geometry(NULL) %>%#removing geometric object
  dplyr::select(QDS, Taxon) %>% #select for QSD and taxon
  unique() %>% #only unique taxon per QDS
  group_by(QDS) %>% #grouped by QDS
  summarise(Narrow_Range_endemics = n()) #Count of the number of species present per QDS 


#Joining the two data sets and omiting nas
Heat.Mapdat <- left_join(qds, Heat.Mapdat, by = c("Name"= "QDS")) %>%
  na.omit()


#The crop the sf object using st_crop then remove all the NAs from the data 
Heat.Mapdat <- st_crop(Heat.Mapdat, Dom.geo)

#Return all facultative wetland dependent narrow range endemics 

Heat.Mapdat <- left_join(Heat.Mapdat, Richness.FCTNRE.QDS, by = "Name") 

#Return all facultative wetland dependent narrow range endemics 

Heat.Mapdat <- left_join(Heat.Mapdat, Richness.ObNRE.QDS, by = "Name")

#Add full species in the cfr 

#Remove all NAs from the Lontitude and latitude columns as well as the QDS column
species_richness_QDS <- species_richness_QDS %>% filter(!is.na(lon)) 


#assign coordinate reference system to the new brahms data for plotting
#species_richness_QDS <- st_as_sf(species_richness_QDS, coords = c("lon", "lat"), crs = 4326)


# Apply the transformation to the 'qdgc' column
#species_richness_QDS$qdgc <- transform_qdgc(species_richness_QDS$qdgc)

species_richness_QDS = species_richness_QDS %>% 
  filter(!(region == "SWAFR")) %>% 
  filter(!is.na(QDS_richness))

species_richness_QDS$qdgc <- sub("E0", "", species_richness_QDS$qdgc)

species_richness_QDS$qdgc <- gsub("S", "", species_richness_QDS$qdgc)

#species_richness_QDS$q <- str_sub(species_richness_QDS$qdgc, start = 3, end = 4)  # Extract the middle part
species_richness_QDS$qdgc <- paste0(str_sub(species_richness_QDS$qdgc, start = 3), 
                                    str_sub(species_richness_QDS$qdgc, start = 1, end = 2), 
                                    str_sub(species_richness_QDS$qdgc, start = 4))  # Rearrange the string

species_richness_QDS$qdgc <- paste0(str_sub(species_richness_QDS$qdgc, end = 2),
                                    str_sub(species_richness_QDS$qdgc, start = 5, end = 6),
                                    str_sub(species_richness_QDS$qdgc, start = 8))
#only keep the columns we want 
species_richness_QDS <- species_richness_QDS %>% dplyr:: select(qdgc,QDS_richness) %>% rename("Name"="qdgc")

#Leftjoin to only keep QDS richness 
Heat.Mapdat<- left_join(Heat.Mapdat, species_richness_QDS, by = "Name")


#Turn the NAs into blanks spaces within the grid
Heat.Mapdat <- Heat.Mapdat %>% 
  mutate(Facultative_species = coalesce(Facultative_species, 0))

#Repeat
Heat.Mapdat <-Heat.Mapdat %>% 
  mutate(Obligate_species = coalesce(Obligate_species, 0))

#Important plot to visualize the data 
ggplot()+
  geom_sf(data = Heat.Mapdat, aes(fill = Total_species_richness, colour = Total_species_richness), lwd = 0)+
  geom_polygon()+
  theme(panel.background = element_rect(fill = "white"))+
  scale_x_continuous(limits = c(16, 28))+
  scale_y_continuous(limits = c(-29, -36))

#Rename the column names 
Heat.Mapdat <- Heat.Mapdat %>% 
  rename(Total_species_richness ="QDS_richness", qdgc = "Name")

#Remove the awkward grid in the ocean 
Heat.Mapdat <- Heat.Mapdat %>% filter(!is.na(Total_species_richness))

#Save Heat.Mapdat
saveRDS(Heat.Mapdat, file = "Data/CFR_spp.")

###################### Proceed to Water and energy script  (Analysis) 


