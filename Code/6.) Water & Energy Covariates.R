#############################################################################
######## Code for analysis of water - energy  as response variable. 
##############################################################################
######## Compiled by Kagiso Nhlapo
######## Last edited: 30 July 2023
##############################################################################
##############################################################################
###Steps:
###1) Get libraries etc
###2) Read  all of the rasters using the rast() function.
#### from: (https://www.worldclim.org/data/worldclim21.html#google_vignette) read: Mean annual
####  temperature, mean annual rainfall, mean dry seasonal rainfall
###4) extract them using QDS shapefile.
###5) Join all 3 covariates to the QDS data.
###6) set the geometry of the QDS data set to NULL so that we can later left join the data
###7) Left join narrow range endemics (NREs)and QDS dateframe,using QDS and qdgc to give us all the covariates with respect to NREs 
###8) Next count the number of species in unique QDS.
###9) Compare if all the variables are autocorrelated?
##10) Fit a Poisson GLM  (With the response as the number of narrow endemic species)
##11) Fit Quasipossion if neccesary 
##############################################################################

##############################################################################
### Get libraries

library(tidyverse)
library(readxl)
library(readr)
library(sf)
library(units)
library(terra)
library(mapview)
library(viridis)
#library(exactextractr)
#library(worldclim)
library(devtools)
library(geodata)
#library(ClimDatDownloadR)
#library(httr)


#if(!require(devtools)) install.packages("devtools")
#library(devtools)
#devtools::install_github("HelgeJentsch/ClimDatDownloadR")


#Read all of the saved data from the exploratory data collation and wrangling (i.e, NREs, CapeFR and qds).
#CapeFR_for_heatmap <- readRDS(file = "Data/CapeFR_for_heatmap.rds")
#NREs <- readRDS(file = "Data/NREs.rds")
#qds <- readRDS("Data/qds.rds")
CFR_data <- readRDS("Data/CFR_spp.")
#Dom.geo <- readRDS("Data/Dom.geo")


############### clean QDS

#unique_qds <- qds %>%
  #dplyr::  group_by(qdgc) %>%
#dplyr:: summarise(pentad = first(pentad)) %>%
  #left_join(qds, by = "QDS") %>%
  #st_cast("MULTIPOLYGON") 

##############################################################################

### Annual Mean Temperature raster cleaning 

#Reading the Annual Mean Temperature raster straight from: (https://www.worldclim.org/data/worldclim21.html#google_vignette)
#BIO1 = Annual Mean Temperature
#Jasper's working directory might be different, so change the path accordingly.
#Read the stacked layers
Clim.var <- geodata:: worldclim_country("South Africa", var="bio", path="C:/Users/NHLKAG001/Documents/GitRepo/hydrorefugia/Data")

#BIO1 = Annual Mean Temperature
#Get the Annual Mean Temperature layer 
Annual_Temperature <- Clim.var$wc2.1_30s_bio_1

#intersect the raster with the QDS layer , so extract them using the QDS shapefile
Annual_Temperature <- terra::extract(Annual_Temperature,CFR_data)

#Rename the columns for convenience
names(Annual_Temperature) <- c("ID","Mean_annual_Temperature")

#Removing Na values for the covariate 
#Annual_Temperature$Mean_annual_Temperature[is.na(Annual_Temperature$Mean_annual_Temperature)] <- 0

#Aggregate the IDs so that there is one average temperature per QDS
Mean_annual_Temperature<- aggregate(Mean_annual_Temperature ~ ID, data = Annual_Temperature, mean)

#Aggregate the IDs by median temperature per QDS
Median_annual_Temperature<- aggregate(Mean_annual_Temperature ~ ID, data = Annual_Temperature, median)

#Rename the columns for convenience
names(Median_annual_Temperature) <- c("ID","Median_annual_Temperature")


CFR_data$ID <- 1:192

#Join data
#Mean
CFR_data <- left_join(CFR_data,Mean_annual_Temperature, by = "ID")
#Median
CFR_data <- left_join(CFR_data,Median_annual_Temperature, by = "ID")



#remove data we dont want 
rm(Annual_Temperature, Mean_annual_Temperature, Median_annual_Temperature)

##############################################################################

### Annual Mean Precipitation

#Get Annual Mean Rainfall from Bioclimatic variable
#BIO12 = Annual Precipitation
#Read the stacked layerAnnual_Rainfall
Annual_Rainfall <- Clim.var$wc2.1_30s_bio_12

#intersect the raster with the QDS layer , so extract them using the QDS shapefile
Annual_Rainfall <- terra:: extract(Annual_Rainfall,CFR_data)

#Rename the columns for convenience
names(Annual_Rainfall) <- c("ID","mean_annual_Rainfall")

#Removing Na values in the covariate 
#Annual_Rainfall$Annual_Rainfall[is.na(Annual_Rainfall$Annual_Rainfall)] <- 0

#Aggregate the IDs so that there is one average Annual_Rainfall per QDSAnnual_Rainfall<- aggregate(Annual_Rainfall ~ ID, data = Annual_Rainfall, mean)
mean_Annual_Rainfall<- aggregate(mean_annual_Rainfall ~ ID, data = Annual_Rainfall, mean)

#Aggregate the IDs so that there is one median Annual_Rainfall per QDS Annual_Rainfall<- aggregate(Annual_Rainfall ~ ID, data = Annual_Rainfall, mean)
median_Annual_Rainfall<- aggregate(mean_annual_Rainfall ~ ID, data = Annual_Rainfall, median)

#Rename the columns for convenience
names(median_Annual_Rainfall) <- c("ID","median_Annual_Rainfall")


#Join data
#Mean
CFR_data <- left_join(CFR_data,mean_Annual_Rainfall, by = "ID")
#Median
CFR_data <- left_join(CFR_data,median_Annual_Rainfall, by = "ID")

#rm objects from memory 
rm(Annual_Rainfall, mean_Annual_Rainfall, median_Annual_Rainfall)

##############################################################################
###  mean Precipitation of Driest Quarter

#Get the mean dry seasonal rainfall, from Bioclimatic variables
#BIO17 = Precipitation of Driest Quarter
#Read the stacked variable 
Dry_Rainfall <- Clim.var$wc2.1_30s_bio_17


#intersect the raster with the QDS layer , so extract them using the QDS shapefile
Dry_Rainfall <- terra:: extract(Dry_Rainfall,CFR_data)

#Rename the columns for convenience
names(Dry_Rainfall) <- c("ID","Dry_Rainfall")

#Removing Na values in the covariate
#Dry_Rainfall$Dry_Rainfall[is.na(Dry_Rainfall$Dry_Rainfall)] <- 0

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Mean_Dry_Rainfall<- aggregate(Dry_Rainfall ~ ID, data = Dry_Rainfall, mean)

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Median_Dry_Rainfall<- aggregate(Dry_Rainfall ~ ID, data = Dry_Rainfall, median)

#add the values to the QDS data
CFR_data$Mean_Dry_Rainfall <- Mean_Dry_Rainfall$Dry_Rainfall

#add the values to the QDS data
CFR_data$Median_Dry_Rainfall <- Median_Dry_Rainfall$Dry_Rainfall

#Remove all unwanted data
rm(Dry_Rainfall, Mean_Dry_Rainfall, Median_Dry_Rainfall, Clim.var)


#PET data from Chelsa (https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2F)
#This data is from 1981-2010
# Create an empty list to store the rasters
raster_stack <- rast()

# Loop through the file names and read the rasters
for (i in 1:12) {
  # Construct the file name
  filename <- paste0("/Users/NHLKAG001/Downloads/CHELSA_pet_penman_", sprintf("%02d", i), "_1981-2010_V.2.1.tif")
  
  # Read the raster
  raster <- rast(filename)
  
  # Stack the raster
  raster_stack <- c(raster_stack, raster)
}

raster_stack <- sum(raster_stack)
# Extract CRS from the sf object
#sf_crs <- st_crs(unique_qds)

# Set the CRS of the raster to match the sf object
#raster_stack <- resample(raster_stack, sf_crs)

#Pet extract using QDS
PET <-terra :: extract(raster_stack,CFR_data)

#Rename the columns for convenience
names(PET) <- c("ID","PET")

#Removing Na values in the covariate 
#PET$PET[is.na(PET$PET)] <- 0
#PET$PET[is.na(PET$PET)] <- 0

#Aggregate the IDs so that there is one PET per QDS
Mean_PET<- aggregate(PET ~ ID, data = PET, mean)

#Aggregate the IDs so that there is one median Annual_Rainfall per QDS Annual_Rainfall<- aggregate(Annual_Rainfall ~ ID, data = Annual_Rainfall, mean)
Median_PET<- aggregate(PET ~ ID, data = PET, median)

#add the values to the QDS data, Annual_Rainfall will show up as columns
CFR_data$Mean_PET <- Mean_PET$PET

#add the values to the QDS data, Annual_Rainfall will show up as columns  
CFR_data$Median_PET <- Median_PET$PET

##############################################################################
### Left join narrow range endemics and QDS dateframe

#remove geometry from the the capeFR_for_heatmap which we need this for the QDS columb from the Origina Brahms data 
#CapeFR_for_heatmap <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#returns all records from the left table (table1), and the matching records from the right table (table2)
#water_ene <- left_join(NREs, CapeFR_for_heatmap, by = "Taxon") 

#Return all the columns we want to work with
#water_ene <- water_ene  %>% 
  #dplyr:: select( "QDS","QDScount") 

#Setup our dataset with a response and all 3 covariates
#By grouping by QDS and having counts species richness per QDS

#water_ene  <- water_ene  %>% #original data from 
  #st_set_geometry(NULL) %>%
  #dplyr::select(QDS,Annual_Mean_Temperature,Annual_Rainfall, Dry_Rainfall) %>% #select for QSD and 3 covariates
  #unique() %>% #only unique taxon per QDS
  #group_by(QDS) %>% #grouped by QDS
  #summarise(species_richness = n())#no species per QDS

##############################################################################

#returns all records from the left table (table1), and the matching records from the right table (table2)
#water_ene <- left_join(water_ene, unique_qds, by = c("QDS"="qdgc")) %>% filter(!is.na(species_richness)) %>%
  #st_set_geometry(NULL) %>%
  #dplyr:: select("species_richness", "QDS","Dry_Rainfall","Annual_Rainfall","Annual_Mean_Temperature")

#Correlation plots for 3 the covariates 
#pairs(~ Dry_Rainfall + Annual_Rainfall + Annual_Mean_Temperature , data = water_ene)

#Cor-test for further investigation between Dry_Rainfall and Annual_Rainfall
#As expected  mean Precipitation of Driest Quarter and Mean Annual_Rainfall are all highly correlated 
#cor.test(unique_qds$Dry_Rainfall, unique_qds$Annual_Rainfall, method = c("spearman"))
#cor(na.omit(water_ene[,-2]))

##############################################################################
##set up a GLM with possion as its family
#Fit a GLM with species_richness as the response with Annual_Mean_Temperature and Dry_Rainfall as the x variables 
#Mod.1 <- glm(species_richness~ Annual_Rainfall + Annual_Mean_Temperature, family = poisson , data = water_ene)
#summary(Mod.1)

#Try the quasipossion model
#Mod.1.1 <- glm(species_richness~ Dry_Rainfall + Annual_Mean_Temperature, family = quasipoisson , data = water_ene)
#summary(Mod.1.1)

#Model checks 

#Model checks for the first model
#plot(Mod.1)
#Model check for the quasiposson 
#plot(Mod.1.1)


####################### Proceed to 8) Heterogeneity script 

