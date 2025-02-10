#### Work flow ####

####Complied by Kagiso Nhlapo 
#### MS C student Working on Hydrologic Refugia spatial characteristics 
#### Last edited 12 October 2022

####   To do  ####
###1) Get libraries etc
###2) Read in locality data and clean
###3) Read in spatial data, match and extract (Red-list and Brahms data sets)
###4) Clean data for QDS <= 4 and CFR 
###5) Use CFR Mask to to find species with QDS less than 4 that occur in the CFR
###6) Read the QDS mask and filter for species so that no duplicates show up in one cell
###7) Use QDS raster layer of overlay the points and create the NRE, wetland and MAP spatial heat maps


####   Get packages from libraries ####
#library(rgdal)
library(tidyverse)
library(readxl)
library(ggpubr)
library(readr)
library(sf)
library(units)
library(terra)
library(mapview)
#library(sp)

#### Data cleaning Brahms and Tspp data ####

#Read SANBI data from http://newposa.sanbi.org/sanbi/Explore
brahms <- read_excel("~/MSc Refugia/Data/2022-08-11_064413137-BRAHMSOnlineData.xlsx") 
View(Brahms)

#get threatened species data and filter by data quality
tspp <- read_delim("Data/WC_TOCC.txt", delim = "\t")

tspp <- st_as_sf(tspp, coords = c("Long", "Lat"), crs = 4326) 

####   Filter By QDS ####
#Summarize Brahms data to count number of QDS per species and filter for <5
sppQDS <- brahms %>% group_by(Taxon) %>% summarize(QDScount = length(unique(QDS, na.rm = T)))


#Filtering QDS <= 4,5 and 6 too
#ppQDS %>% filter(QDScount < 4) %>% nrow()
sppQDS4 <- sppQDS %>% filter(QDScount < 4)


# Extracted full Brahms data for species with <5 QDS records
sppQDSdat <- brahms %>% filter(Taxon %in% sppQDS4$Taxon)


### Match new names to TSP data to identify new species to add
newsppQDS <- anti_join(sppQDSdat, tspp, by = c("Taxon"))
attach(newsppQDS)

#Remove rows with "NA' in the longitude colunm
newsppQDS= newsppQDS %>% filter(!is.na(Longitude))

####   Figure 1 ####
newplot <- st_as_sf(newsppQDS, coords = c("Longitude", "Latitude"), crs = 4326) 

mapView(x = newplot, legend = F, label = "Taxon")


####   Overlay CFR Mask on newplot #### 

BDhspot <- sf::st_read("hotspots_2016_1.shp")
####   Figure 2 ####
BDMask <- BDhspot %>% filter(NAME == "Cape Floristic Region")

####   Extracting QDs and ploting ####
CapeFR <- st_intersection(newplot, BDMask)

mapView(x = CapeFR, legend = F, label = "Taxon")


#### NRE Heatmap ####


####  Extracting lat and long coordinates from the geomtery column in the CapeFR Object ####
Capexy <- CapeFR %>% tidyr::extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) 

#Removing the extra geometry column that forms 

Capexy = Capexy[,1:5]

####  Figure 3 ####
#Now we plot the points using ggplot
Capexy1 <- CapeFR %>% ggplot() + geom_sf(aes(fill = QDScount))
ggplot(data=Capexy, aes(lat, lon, fill= QDScount)) + geom_point()
Capexy1 + geom_density2d_filled(data=Capexy,aes(lat, lon, fill= QDScount))

##Now we plot the same points only this time we geom_density2d_filled() function 
WMap = ggplot(data=Capexy,aes(lat, lon))+ geom_point() + geom_density2d_filled(data=Capexy,aes(lat, lon, fill=QDScount))

Capexy = Capexy[,1:7]

####  Figure 3 ####
#Now we plot the points using ggplot
Capexy <- CapeFR %>% ggplot() + geom_sf(aes(fill = TYPE))
ggplot(data=Capexy, aes(lat, lon, fill= TRUE)) + geom_point()


##Now we plot the same points only this time we geom_density2d_filled() function 
WMap = ggplot(data=Capexy,aes(lat, lon))+ geom_point() + geom_density2d_filled(data=Capexy,aes(lat, lon, fill=, alpha=..level..))

####  Figure 4 ####
WMap

#
CapeFR %>% group_by(QDS) %>% summarise(Taxonrichness = n(Taxon))

#Here we will use the duplicated function to return species that dont show up in more than one pixle
newsppQDS[!duplicated(newsppQDS[,c("Taxon","QDS")]),]

####  QDSlayer #### 

###   Read the QDS mask raster using the st read fucntion and Clean it ####
QDS_Mask <- sf::st_read("qds_pentads.geojson")

#Filter to only include provinces where the CFR occurs
wesKaap <- QDS_Mask %>% filter(province %in% c("Western Cape","Eastern Cape", "Northern Cape"))


#We want to use ggplot so we extract the lat,long from geometry in Weskaap
Cords_Mask1 <-st_coordinates(st_centroid(wesKaap$geometry))
Cords_Mask1 <- data.frame(Cords_Mask1)

wesKaap$X <- Cords_Mask1$X
wesKaap$Y <- Cords_Mask1$Y


#plot the QDS layer 
wesKaap.map =ggplot(data = wesKaap, aes(x = X, y = Y)) +
  geom_polygon(colour = "blue", fill = "grey70", aes(group = qdgc))+
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme_bw()

#plot the points overlayed with map to produce a heat map

wesKaap.map + geom_sf(data = CapeFR, aes(fill = QDScount))

b =  ggplot() + geom_sf(data = CapeFR, aes(fill = QDScount))+
  
b + geom_polygon(colour = "white", fill = "grey70",aes(x = X, y = Y, group = qdgc))+
  coord_equal()




#### Wetland species NRE Heatmap ####

Obligate_wetland_species %>% names()

attach(Obligate_wetland_species)


wetObspps <- Obligate_wetland_species %>% filter(WetlandDependence...3 == 1)

wetObspps

CPwetland= anti_join(wetObspps, newsppQDS, by = c("Taxon"))

CPwetland


####  Wetland shape file ####
wetland <- sf::st_read("wriall500.shp")
wetland %>% mapview()

ggplot() +
  geom_sf(data=Capexy,aes(lat, lon, fill = QDScount))+
stat_density2d(aes(fill = QDScount), alpha = 0.5, geom = "polygon") + scale_fill_viridis_c()

ggplot()+ geom_point() + geom_density2d_filled(data=CapeFR,aes(fill=QDScount))












