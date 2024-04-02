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
library(sf)

if(Sys.getenv("USER") == "jasper") {
  bigdat <- "/Users/jasper/Dropbox/UCT/Students/KagisoNhlapo/BigDataShare"}

#Kagiso sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "") {
  bigdat <- "/Users/NHLKAG001/Dropbox/BigDataShare"}

#clear data used by R
gc()


#Load saved files from script 1
CapeFR_for_heatmap <- readRDS("Data/CapeFR_for_heatmap.rds")
NREs <- readRDS("Data/NREs.rds")
#SAspp <- readRDS("Data/SAspp.rds")
qds <- readRDS("Data/qds")

#### Read and cleaning Wetland spp data ####

#Read wetland species 
Wetlandsp <- read_excel("Obligate_wetland_species.xlsx")

Wetlandsp = Wetlandsp%>% mutate(Taxon = str_extract(Taxon, "^\\S+\\s+\\S+"))


### #obligate wetland species ####


#Keep all of the Obligate species
ObWetland <- Wetlandsp %>% filter(WetlandDependence...15 %in% c("Obligate wetland plant"))

#to the clean the taxon column we join the two data sets
#ObWetland <- semi_join(SAspp, ObWetland, by = c("FULLNAME" = "Taxon"))

#Clean up the taxon column 
ObWetland <- ObWetland %>% mutate(Taxon = str_extract(Taxon, "^\\S+\\s+\\S+"))

#Create an extra column by only including the genus and sp name 
#ObWetland <- ObWetland %>% mutate(Taxon_ = paste(Genus, SP1) %>% trimws())


#Return all narrow range endemics unique to Wetland species 
ObNRE <-semi_join(NREs, ObWetland, by = c("Taxon" = "Taxon"))
#ObNRE <- left_join(ObWetland, NREs, by = c("Taxon_" = "Taxon"))


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

#### Facultative narrow range endemics ####


FctWetland <- Wetlandsp %>% filter(WetlandDependence...15 %in% c("Facultative wetland plant","Facultative wetland plant–",
                                                                 "Facultative wetland plant+","Facultative wetland plant +",
                                                                 "Facultative wetland plant –","Facultative wetland plant?",
                                                                 "Facultative wetland plant\037","Facultative wetland.",
                                                                 "Facultative wetland+","Facutative wetland plant",
                                                                 "Facultative wetland plant plant–", "Facultative wetland +",
                                                                 "Facultative wetland plant.", "\r\nFacultative wetland plant–", "1",
                                                                 "Facultative wetland plant– or Facultative wetland plant or Obligate wetland plant"))

#clean the taxon column
#FctWetland <- semi_join(SAspp, FctWetland, by = c("FULLNAME"= "Taxon"))

#FctWetland <- FctWetland %>% mutate(Taxon = paste(Genus, SP1) %>% trimws())

#Clean up the taxon column 
FctWetland <- FctWetland %>% mutate(Taxon = str_extract(Taxon, "^\\S+\\s+\\S+"))


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

#removing turning mutltipoint to point
ObNRE = st_cast(ObNRE, "POINT", group_or_split = TRUE)

#checking if all point are indeed point and not multipoint
st_geometry_type(ObNRE)

#return only Taxon, QDS, QDScount columns
ObNRE <- ObNRE %>% dplyr::select("Taxon","QDScount","geometry")

#saving data as a RDS objects, that are going to used in later scripts 
saveRDS(FctNRE, file = "Data/FctNRE.RDS")
saveRDS(ObNRE, file =  "Data/ObNRE.RDS")

#remove all unwanted data from the environment 
rm(CapeFR_for_heatmap,FctNRE, FctWetland, NREs,ObNRE, ObWetland, 
   qds, SAspp, Wetlandsp, bigdat, duplicated_taxa_F, duplicated_taxa_O)

###################### Proceed to script four (4a, spatial heatmaps wetlands) 















