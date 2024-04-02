#############################################################################
######## Code for analysis of heterogeneity as response variable. 
##############################################################################
######## Compiled by Kagiso Nhlapo
######## Last edited: 17 July 2023
##############################################################################
##############################################################################
###Steps:
###1) Get libraries etc
###2) Read  all of the rasters using the rast() function.
#### from: (https://www.worldclim.org/data/worldclim21.html#google_vignette) read: Mean annual
####  temperature, mean annual rainfall, mean dry seasonal rainfall
###3) Find the range per QDS then the roughness per QDS of the 3 covariates (In total = 6), then find the dominate geology using (Diversity)
###4) extract them using QDS shapefile.
###5) Join all 6 covariates to the QDS data.
###6) set the geometry of the QDS data set to NULL so that we can later left join the data
###7) Left join narrow range endemics (NREs)and QDS dateframe,using QDS and qdgc to give us all the covariates with respect to NREs 
###8) Next count the number of species in unique QDS.
###9) Compare if all the variables are autocorrelated?
##10) Fit a Poisson GLM  (With the response as the number of narrow endemic species)
##11) Fit Quasipossion if neccesary 

#Read all of the saved data from the exploratory data collation and wrangling (i.e, NREs, CapeFR and qds).
#Richeness.NRE.QDS <- readRDS("Data/Richeness.NRE.QDS")

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
library(raster)
library(geodata)
library(vegan)
#library(exactextractr)
####Read exploratory data for analysis (i.e, NREs, CapeFR)
#loading files saved from Script 1
#CapeFR_for_heatmap <- readRDS(file = "Data/CapeFR_for_heatmap.rds")
#NREs <- readRDS(file = "Data/NREs.rds")
#qds <- readRDS("Data/qds.rds")

#Return only unique QDS
#unique_qds <- qds %>% distinct(qdgc, .keep_all = TRUE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Range------------------------------------------------

##############################################################################

### Range Temperature raster cleaning 

#Reading the Annual Mean Temperature raster straight from: (https://www.worldclim.org/data/worldclim21.html#google_vignette)
#BIO1 = Annual Mean Temperature
#Jasper's working directory might be different, so change the path accordingly.
#Read the stacked layers
Clim.var <- worldclim_country("South Africa", var="bio", path="C:/Users/NHLKAG001/Documents/GitRepo/hydrorefugia/Data")

#BIO1 = Annual Mean Temperature
#Get the Annual Mean Temperature layer 
Annual_Mean_Temperature <- Clim.var$wc2.1_30s_bio_1

#intersect the raster with the QDS layer , so extract them using the QDS shapefile
Annual_Mean_Temperature <- terra::extract(Annual_Mean_Temperature,CFR_data)

#Rename the columns for convenience
names(Annual_Mean_Temperature) <- c("ID","temperature_range")

#Removing Na values for the covariate 
#Annual_Mean_Temperature$temperature_range[is.na(Annual_Mean_Temperature$temperature_range)] <- 0

#Aggregate the QDS so that there is a range for the QDS temperature 
temperature_range <- aggregate(temperature_range ~ ID, data = Annual_Mean_Temperature, FUN = function(x) diff(range(x)))

#Rename the columns for convenience
#names(Annual_Mean_Temperature) <- c("ID","temperature_range")

#add the data the co-variate to the QDS data as "Annual_Mean_Temperature"
CFR_data$Range_Temperature <- temperature_range$temperature_range

##############################################################################

### Range annual Precipitation

#Get Annual Mean Rainfall from Bioclimatic variable
#BIO12 = Annual Precipitation
#Read the stacked layerAnnual_Rainfall
Annual_Rainfall <- Clim.var$wc2.1_30s_bio_12

#intersect the raster with the QDS layer , so extract them using the QDS shapefile
Annual_Rainfall <- terra:: extract(Annual_Rainfall,CFR_data)

#Rename the columns for convenience
names(Annual_Rainfall) <- c("ID","Annual_Rainfall")

#Removing Na values in the covariate 
#Annual_Rainfall$Annual_Rainfall[is.na(Annual_Rainfall$Annual_Rainfall)] <- 0

#Aggregate the QDS so that there is a range for the QDS Rainfall 
rainfall_range <- aggregate(Annual_Rainfall ~ ID, data = Annual_Rainfall, FUN = function(x) diff(range(x)))

#Rename the columns for convenience
names(rainfall_range) <- c("ID","rainfall_range")

#add the data the co-variate to the QDS data as "Annual_Mean_Temperature"
CFR_data$Range_Rainfall <- rainfall_range$rainfall_range


##############################################################################
###  Range Precipitation of Driest Quarter

#Get the range dry seasonal rainfall, from Bioclimatic variables
#BIO17 = Precipitation of Driest Quarter
#Read the stacked variable 
Dry_Rainfall <- Clim.var$wc2.1_30s_bio_17

#intersect the raster with the QDS layer , so extract them using the QDS shapefile
Dry_Rainfall <- terra:: extract(Dry_Rainfall,CFR_data)

#Rename the columns for convenience
names(Dry_Rainfall) <- c("ID","Dry_Rainfall")

#Removing Na values in the covariate
#Dry_Rainfall$Dry_Rainfall[is.na(Dry_Rainfall$Dry_Rainfall)] <- 0

#Aggregate the QDS so that there is a range for the QDS Rainfall 
Dry_rainfall_range <- aggregate(Dry_Rainfall ~ ID, data = Dry_Rainfall, FUN = function(x) diff(range(x)))

#Rename the columns for convenience
names(Dry_rainfall_range) <- c("ID","Dry_rainfall_range")

#add the data the co-variate to the QDS data as "Annual_Mean_Temperature"
CFR_data$Range_Dry_rainfall <- Dry_rainfall_range$Dry_rainfall_range

#rm all extracted rater datasets 
#rm(Annual_Mean_Temperature, Dry_Rainfall, Annual_Rainfall)

#Get the data, from Chelsa Bioclimatic variables
#Read the stacked variable 

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
PET_Range <- raster_stack

#extract values rainfall values according to the QDS data 
PET_Range <- terra:: extract(PET_Range, CFR_data)

#Removing Na values in the covariate 
#PET_Roughness$roughness[is.na(PET_Roughness$roughness)] <- 0

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
PET_Range <- aggregate(sum ~ ID, data = PET_Range, FUN = function(x) diff(range(x)))

#add the values to the QDS data
CFR_data$PET_Range <- PET_Range$sum



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------roughness--------------------------------------------


###############################################################################

#Range annual temperature

#Get Annual Mean Temperature FROM Bioclimatic variables
#BIO1 = Annual Mean Temperature
Annual_Mean_Temperature <- Clim.var$wc2.1_30s_bio_1

#Determining the difference between the 8 neigbouring pixels 
Roughness_Temperature <- terra::terrain(Annual_Mean_Temperature, "slope")

#extract values rainfall values according to the capefr data 
Roughness_Temperature <- terra:: extract(Roughness_Temperature, CFR_data)

#Removing Na values in the covariates 
#Roughness_Temperature$roughness[is.na(Roughness_Temperature$roughness)] <- 0

#Aggregate the IDs so that there is one average temperature per QDS
Roughness_Temperature<- aggregate(slope ~ ID, data = Roughness_Temperature, mean)

#add the values to the qds data
CFR_data$Roughness_Temperature <- Roughness_Temperature$slope


##############################################################################

### range Annual Precipitation

#Get Annual Mean Rainfall from Bioclimatic variable
#BIO12 = Annual Precipitation
#Read the stacked layer Annual_Rainfall
Annual_Rainfall <- Clim.var$wc2.1_30s_bio_12

#Determining the elevation values of the raster
Roughness_Rainfall <- terra::terrain(Annual_Rainfall, "slope")

#extract values rainfall values according to the QDS data 
Roughness_Rainfall <- terra:: extract(Roughness_Rainfall, CFR_data)

#Removing Na values in the covariate 
#Roughness_Rainfall$roughness[is.na(Roughness_Rainfall$roughness)] <- 0

#Aggregate the IDs so that there is one average Annual_Rainfall per QDS
Roughness_Rainfall <- aggregate(slope ~ ID, data = Roughness_Rainfall, mean)

#add the values to the QDS data, Annual_Rainfall will show up as 
CFR_data$Roughness_Rainfall <- Roughness_Rainfall$slope


##############################################################################
### range Precipitation of Driest Quarter

#Get the dry seasonal rainfall, from Bioclimatic variables
#BIO17 = Precipitation of Driest Quarter
#Read the stacked variable 
Dry_Rainfall <- Clim.var$wc2.1_30s_bio_17

#Determining the elevation values of the raster
Roughness_Dry_Rainfall <- terra::terrain(Dry_Rainfall, "slope")

#extract values rainfall values according to the QDS data 
Roughness_Dry_Rainfall <- terra:: extract(Roughness_Dry_Rainfall, CFR_data)

#Removing Na values in the covariate 
#Roughness_Dry_Rainfall$roughness[is.na(Roughness_Dry_Rainfall$roughness)] <- 0

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Roughness_Dry_Rainfall<- aggregate(slope ~ ID, data = Roughness_Dry_Rainfall, mean)

#add the values to the QDS data
CFR_data$Roughness_Dry_Rainfall <- Roughness_Dry_Rainfall$slope

#Remove all unwanted data when done 
#rm(Roughness_Rainfall,Roughness_Dry_Rainfall,Roughness_Temperature)

##############################################################################
### Roughness pet

#Get the data, from Chelsa Bioclimatic variables
#Read the stacked variable 

PET_Roughness <- raster_stack

#Determining the elevation values of the raster
PET_Roughness <- terra::terrain(raster_stack, "slope")

#extract values rainfall values according to the QDS data 
PET_Roughness <- terra:: extract(PET_Roughness, CFR_data)

#Removing Na values in the covariate 
#PET_Roughness$roughness[is.na(PET_Roughness$roughness)] <- 0

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
PET_Roughness<- aggregate(slope ~ ID, data = PET_Roughness, mean)

#add the values to the QDS data
CFR_data$PET_Roughness <- PET_Roughness$slope

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------Geology----------------------------------------------

#Dominate geology 

#option one have the count of the dominate geology 
#option two have diversity index thing on the geology diversity

##############################################################################
###  Dominant geology 

#Reading the Dominant geology  raster straight from: Hoffmann et al 2015  
#Dom.geo <-terra:: rast(raster("Data/lith_simplif.grd"))

#intersect the raster with the QDS layer , so extract them using the QDS shapefile
#Dom.geo <- terra:: extract(Dom.geo,unique_qds) %>% filter(!is.na(layer))
#unique_qds$domgeo <- exact_extract(Dom.geo,unique_qds, fun = "majority")# %>% filter(!is.na(layer))

#Rename the columns for convenience
#names(Dom.geo) <- c("ID","Dom.geo")
#levels(unique_qds$domgeo)
#Rename Levels 
#class(Dom.geo$Dom.geo)
#Making it a factor 
#unique_qds$domgeo <- as.factor(unique_qds$domgeo)
#Rename factor 
#unique_qds$domgeo<-recode_factor(unique_qds$domgeo, "1" = "quartzitic_sandstone" , "2" = "shale", "3" = "granite", 
                                 #"4" = "calcareous_substrata", "5" = "alluvial_deposits", "6" = "lowland_sands")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------SOIL pH and P ---------------------------------------------

#read the PH data
Soil_ph <-rast("/Users/NHLKAG001/Desktop/Data/pH.tif")

#extract the mean PH values and Median PH values 
Soil_ph <- terra:: extract(Soil_ph,CFR_data)

#Rename the columns for convenience
names(Soil_ph) <- c("ID","pH")

#Removing Na values in the covariate
#Soil_ph$pH[is.na(Soil_ph$pH)] <- 0

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Mean_Soil_ph<- aggregate(pH ~ ID, data = Soil_ph, mean)

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Median_Soil_ph<- aggregate(pH ~ ID, data = Soil_ph, median)

#add the values to the QDS data
CFR_data$Mean_Soil_ph <- Mean_Soil_ph$pH

#add the values to the QDS data
CFR_data$Median_Soil_ph <- Median_Soil_ph$pH


#read the P data
Ext_P <-rast("/Users/NHLKAG001/Desktop/Data/Ext_P_mg_kg.tif")
#extract the mean PH values and Median PH values 
Ext_P <- terra:: extract(Ext_P,CFR_data)

#Rename the columns for convenience
names(Ext_P) <- c("ID","p")

#Removing Na values in the covariate
#Ext_P$p[is.na(Ext_P$p)] <- 0

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Mean_Ext_P<- aggregate(p ~ ID, data = Ext_P, mean)

#Aggregate the IDs so that there is one average Dry_Rainfall per QDS
Median_Ext_P<- aggregate(p ~ ID, data = Ext_P, median)

#add the values to the QDS data
CFR_data$Mean_Ext_P <- Mean_Ext_P$p

#add the values to the QDS data
CFR_data$Median_Ext_P <- Median_Ext_P$p

#saveRDS(unique_qds, file = "Data/unique_qds")
saveRDS(CFR_data, file = "Data/CFR_data.RDS")


##############################################################################
###################### Proceed to Climatic stability script (Analysis)
