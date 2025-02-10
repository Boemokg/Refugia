#Kagiso sinking the dropbox with the local computer 
{bigdat <- "/Users/NHLKAG001/Dropbox/BigDataShare"}
CFR_data_fin <- readRDS("Data/CFR_data_Fin.RDS") #%>% rename(Mean_CloudFQ = Cloud)



#read the raster rather so that we can extract the species richness values from there 
species_richness_QDS  <- read_csv("C:/Users/NHLKAG001/Downloads/doi_10_5061_dryad_8w9ghx3m8__v20210423/species-richness_QDS.csv")


#Remove all NAs from the Lontitude and latitude columns as well as the QDS column
species_richness_QDS <- species_richness_QDS %>% filter(!is.na(lon)) 


#assign coordinate reference system to the new brahms data for plotting
species_richness_QDS <- st_as_sf(species_richness_QDS, coords = c("lon", "lat"), crs = 4326)


# Apply the transformation to the 'qdgc' column
#species_richness_QDS$qdgc <- transform_qdgc(species_richness_QDS$qdgc)

species_richness_QDS = species_richness_QDS %>% filter(!(region == "SWAFR")) %>% filter(!is.na(QDS_richness))


species_richness_QDS$qdgc <- sub("E0", "", species_richness_QDS$qdgc)

species_richness_QDS$qdgc <- gsub("S", "", species_richness_QDS$qdgc)

# Assuming your data frame is named 'your_data' and the column is named 'your_column'
library(stringr)

#species_richness_QDS$q <- str_sub(species_richness_QDS$qdgc, start = 3, end = 4)  # Extract the middle part
species_richness_QDS$qdgc <- paste0(str_sub(species_richness_QDS$qdgc, start = 3), 
                                str_sub(species_richness_QDS$qdgc, start = 1, end = 2), 
                                str_sub(species_richness_QDS$qdgc, start = 4))  # Rearrange the string

species_richness_QDS$qdgc <- paste0(str_sub(species_richness_QDS$qdgc, end = 2),
                          str_sub(species_richness_QDS$qdgc, start = 5, end = 6),
                          str_sub(species_richness_QDS$qdgc, start = 8))

species_richness_QDS =  species_richness_QDS %>% st_set_geometry(NULL)


Heat.mapdat =left_join(Heat.mapdat, species_richness_QDS, by = c("qdgc"= "qdgc")) %>% 
  rename(total_species_richness = QDS_richness) %>% 
  dplyr::select(-region, -hdgc, -dgc, -n_collections)

                    