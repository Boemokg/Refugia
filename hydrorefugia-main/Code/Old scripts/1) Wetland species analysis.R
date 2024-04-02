#### Work flow ####

####Complied by Kagiso Nhlapo 
#### MSC student Working on Hydrologic Refugia spatial characteristics 
#### Last edited 11 November 2022

####   To do  ####
###1) Get libraries etc
###2) Read in wetland species data 
###3) Keep all of the Obligate species 
###4) return only Narrow range species that are present in wetland obligate species
###5) read in the wetland mask then clean 
###6) then filter the species
###7) create heat map


####   Get packages from libraries ####

#get packages from library
library(tidyverse)
library(readxl)

#### Read and cleaning Wetland spp data ####

#Read wetland species 
Wetlandsp <- read_excel("Data/Obligate_wetland_species.xlsx")


### obligate wetland species ###


#Keep all of the Obligate species
ObWetland <- Wetlandsp %>% filter(WetlandDependence...15 %in% c("Obligate wetland plant"))

#to the clean the taxon column we join the two data sets
ObWetland <- semi_join(SAspp, ObWetland, by = "Taxon")

#Create an extra column by only including the genus and sp name 
ObWetland <- ObWetland %>% mutate(Taxon_ = paste(Genus, SP1) %>% trimws())


#Return all narrow range endemics unique to Wetland species 
ObNRE <-semi_join(NREs, ObWetland, by = c("Taxon" = "Taxon_"))
#ObNRE <- left_join(ObWetland, NREs, by = c("Taxon_" = "Taxon"))



#Trim to only include taxon, QDScount and locality 
#ObNRE <- ObNRE[1:3299, 1:3]

#removing duplicate taxa from the data (Validation step)
duplicated_taxa_O <- ObNRE %>%
  group_by(Taxon) %>%
  summarise(n_rows = n()) %>%
  filter(n_rows > 1) %>%
  arrange(desc(n_rows)) %>%
  pull(Taxon)

#the duplicated taxa characters
duplicated_taxa_O

#removing duplicate taxa from the data (Validation step)
ObNRE <- ObNRE %>%
  filter(!(Taxon %in% duplicated_taxa_O))

#### Facultative narrow range endemics ###


FctWetland <- Wetlandsp %>% filter(WetlandDependence...15 %in% c("Facultative wetland plant","Facultative wetland plant–",
                                                              "Facultative wetland plant+","Facultative wetland plant +",
                                                              "Facultative wetland plant –","Facultative wetland plant?",
                                                              "Facultative wetland plant\037","Facultative wetland.",
                                                              "Facultative wetland+","Facutative wetland plant",
                                                              "Facultative wetland plant plant–", "Facultative wetland +",
                                                              "Facultative wetland plant.", "\r\nFacultative wetland plant–"))

#clean the taxon column
FctWetland <- semi_join(SAspp, FctWetland, by = "Taxon")

FctWetland <- FctWetland %>% mutate(Taxon = paste(Genus, SP1) %>% trimws())


#Return all narrow range endemics unique to Wetland species 
FctNRE <-semi_join(NREs, FctWetland, by = "Taxon") 

#Trim to only include taxon, QDScount and locality 
#FctNRE <- FctNRE[1:3207, 1:3]

#check for duplicates 
duplicated_taxa_F<- FctNRE %>%
  group_by(Taxon) %>%
  summarise(n_rows = n()) %>%
  filter(n_rows > 1) %>%
  arrange(desc(n_rows)) %>%
  pull(Taxon)

#removing duplicate taxa from the data (Validation step)
FctNRE <- FctNRE %>%
  filter(!(Taxon %in% duplicated_taxa_F))

#read in wetland mask
Wet_mask = st_read("Data/NBA2018_National Wetland Map 5 and confidence map/NBA2018.shp")

#transform the wetland layer coordinate reference system to WGS84 Coordinate reference Systems 
Wet_mask <- st_transform(Wet_mask, crs = 4326)

#### validation steps ####

#intersect the mask with the nre species to see which species fall within the wetland habitats (Obligate and Facltative)
val.one <- sf :: st_intersection(NREs, Wet_mask)

#























