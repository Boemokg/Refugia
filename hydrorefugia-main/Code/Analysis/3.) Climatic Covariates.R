#############################################################################
######## Code for analysis of heterogeneity as response variable. 
##############################################################################
######## Compiled by Kagiso Nhlapo
######## Last edited: 31 October 2023
##############################################################################
##############################################################################
###Steps:
###1) Get libraries etc
###2) Read  all of the rasters using the rast() function.
#### from: (see http://www.earthenv.org/metadata/Cloud_DataDescription.pdf for details cloud rasters)
###3) stack then over lay these rasters
###4) extract them using QDS shapefile.
###5) Join them to the QDS data.
###6) repeast this step for the 3 other raters 
###7) For the water layer, we have to convert that to a raster (Rasterize), then extract some of the vaules 
###8) Next count the number of species in unique QDS.
###9) Compare if all the variables are autocorrelated?
##10) Fit a Poisson GLM  (With the response as the number of narrow endemic species)
##11) Fit Quasipossion if neccesary 

#Jasper sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "jasper") {
  bigdat <- "/Users/jasper/Dropbox/UCT/Students/KagisoNhlapo/BigDataShare"}

#Kagiso sinking the dropbox with the local computer 
if(Sys.getenv("USER") == "") {
  bigdat <- "/Users/NHLKAG001/Dropbox/BigDataShare"}

#Read all of the saved data from the exploratory data collation and wrangling (i.e, NREs, CapeFR and qds).


#loading files saved from Script 1
#unique_qds <- readRDS("Data/unique_qds.rds")
CFR_data <- readRDS ("Data/CFR_data.RDS")



#### Libraries ####
library(tidyverse)
library(sf)
library(terra)
#library(data.table)
#library(ggplot2)
library(raster)
library(exactextractr)


#### January rain fall mean ####

#Read the cloud rasters into r - see http://www.earthenv.org/metadata/Cloud_DataDescription.pdf for details
Map_1<-rast("C:/Users/NHLKAG001/Downloads/MODCF_monthlymean_01.tif")
Map_2 <-rast("/Users/NHLKAG001/Downloads/MODCF_monthlymean_02.tif")
Map_12 <-rast(paste0(bigdat,"/MODCF_monthlymean_12.tif"))

#Stack the raster layers 
Cloud.Maps <- c(Map_1,Map_2,Map_12)

#Find the average 
Cloud.Maps<-mean(Cloud.Maps)

#Check CRS and check?
crs(Cloud.Maps)

#extract values 
Cloud.Maps <- terra:: extract(Cloud.Maps, CFR_data)

#add the values to the capefr data
#CapeFR_for_heatmap$sum <- Cloud.Maps$sum

#remove geometry from the the capeFR_for_heatmap
#CapeFR_for_heatmap <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#Aggregate the QDS so that there is a range for the QDS temperature 
Cloud.Maps.mean <- aggregate(mean ~ ID, data = Cloud.Maps, mean)

#Aggregate the QDS so that there is a range for the QDS temperature 
Cloud.Maps.median <- aggregate(mean ~ ID, data = Cloud.Maps, median)


#add the cloud variable 
CFR_data$Mean_CloudFQ <- Cloud.Maps.mean$mean
CFR_data$Median_CloudFQ <- Cloud.Maps.median$mean

#save data
#saveRDS(CFR_data, file = "Data/CFR_data.RDS")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------CSI ---------------------------------------------

#Running the fig share file from the CSI study 
CSI <- rast("C:/Users/NHLKAG001/Desktop/Data/csi_past.tif")

#add the values to the capefr data
#CapeFR_for_heatmap$sum <- Cloud.Maps$sum

#remove geometry from the the capeFR_for_heatmap
#CapeFR_for_heatmap <- CapeFR_for_heatmap %>% st_set_geometry(NULL)

#Aggregate the QDS so that there is a range for the QDS temperature 
#CSI_mean <- aggregate(sum ~ ID, data = CSI, mean)

#Aggregate the QDS so that there is a range for the QDS temperature 
#CSI_median <- aggregate(sum ~ ID, data = CSI, median)


#add the cloud variable 

CSI <-extract(CSI, CFR_data)

#CSI$csi_past[is.na(CSI$csi_past)] <- 0

#CSI.mean <- aggregate(csi_past ~ ID, data = CSI, mean)

CSI.mean <- aggregate(csi_past ~ ID, data = CSI, mean, na.rm = FALSE)


#Aggregate the QDS so that there is a range for the QDS temperature 
CSI.median <- aggregate(csi_past ~ ID, data = CSI, median)


#add the cloud variable 
CFR_data$Mean_CSI <- CSI.mean$csi_past
CFR_data$Median_CSI <- CSI.median$csi_past


rm(Cloud.Maps, Cloud.Maps.mean, Cloud.Maps.median, CSI, CSI.mean, CSI.median, Map_1, Map_12, Map_2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------------------------wetland ---------------------------------------------


#read the national wetland layer 
#CFR <- st_read("C:/Users/NHLKAG001/Desktop/CFR (1).kml")

#`Read NFEPA_SQ4s_AEA' 
NFEPA <- st_read("NWM6_Beta_V3_20230714_CFR/NWM6_Beta_V3_20230714_CFR.dbf")


#only keep :"DEPR","RIVER","SEEP","CVB"
wetlandtypes <- c("Depression","RIVER","Seep","Channelled valley-bottom", 
                  "Floodplain", "Unchannelled valley-bottom","Wetland Flat")

#Filter that from data
NFEPA <- NFEPA %>% filter (HGM_type %in% wetlandtypes)


#CFR mask 
#cfr <- st_read("C:/Users/NHLKAG001/Downloads/CFR.KML")

#Transform
NFEPA <- st_transform(NFEPA, crs = crs(CFR_data))

#Crop Nfepa using CFR mask
#NFEPA_New <- st_crop(NFEPA, cfr)
#Crop the Unique_qds data using the CFR mask, this to ensure we all only Cape Floristic region data 
#First ensure CRS is the same between the two
#CFR <- st_transform(CFR, st_crs(Heat.mapdat))

#Now intersect
#unique_qds <- st_intersection(CFR, Heat.mapdat)


#Then we will intersect the NFEPA and unique QDS, here we will only have intesected polgons of the two shape files 
#Check the two shapefile have the same crs
#crs(NFEPA)==crs(Heat.mapdat)

#Now we make crs the same 
#NFEPA_New <- st_transform(NFEPA_New, st_crs(Heat.mapdat))

#How many overlaps we have (20351)
#sum(st_overlaps(Heat.mapdat,NFEPA,sparse = F))
CFR_data_fin = CFR_data
#Find the area of the unique QDS and add it on the on the dataframe
CFR_data_fin$qdgc_Area <- CFR_data_fin %>% st_area() %>% as.numeric()

#checking if the geometries are valid
st_is_valid(CFR_data_fin)

#make valid
NFEPA <- st_make_valid(NFEPA)

#Checking 
st_is_valid(NFEPA)

#Then making valid 
CFR_data_fin <- st_make_valid(CFR_data_fin)

#Here we are going to find the intercept of the two spatial data frames,
#then we are going to find the area of this intercept then find its proportion 
#then find a percentage 

#Intersection of the data
Wetland_data <- st_intersection(CFR_data_fin,NFEPA) 

#fix the edge grids
Wetland_data <- st_make_valid(Wetland_data)

wetland_areas<-Wetland_data%>%
  arrange(qdgc) %>%
  mutate(area=as.numeric(st_area(.)))

wetland_areas_per_QDGC<-wetland_areas %>%
  st_drop_geometry() %>%
  group_by(qdgc) %>%
  summarise(intersect_area = sum(area),
            qdgc_area=unique(qdgc_Area),
            percent_wetland=intersect_area*100/qdgc_area)


#Leftjoin data 
CFR_data <- left_join(CFR_data, wetland_areas_per_QDGC, by = "qdgc")

saveRDS(CFR_data, file = "Data/CFR_data")

##############################################################################
######proceed to the Manuscript map script under code





