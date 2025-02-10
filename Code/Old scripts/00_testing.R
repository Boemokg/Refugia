##############################################################################
######## Code to explore evidence for hydrologic paleorefugia in the CFR
##############################################################################
######## Compiled by Jasper Slingsby
######## Last edited: 20 March 2022
##############################################################################
##############################################################################
###Steps:
###1) Get libraries etc
###2) Read in locality data and clean
###3) Read in spatial data, match and extract
###
### - Match with Sieben species list?
##############################################################################

##############################################################################
### Get libraries
##############################################################################

library(tidyverse)
library(readr)
library(sf)
library(units)
library(terra)
library(mapview)

##############################################################################
### Get threatened species data and filter by data quality
##############################################################################

## Get data and convert to spatial
tspp <- read_delim("Data/WC_TOCC.txt", delim = "\t")
tspp <- st_as_sf(tspp, coords = c("Long", "Lat"), crs = 4326) 

## Convert precision info to numeric so it can be used for filtering
tspp <- tspp %>% mutate(Precision_m = as.numeric(as.character(Precision_m)))

## Various filter options
#tspp <- tspp %>% filter(Precision_m < 1001)
#tsp_rare <- tspp[which(tspp$'NATIONAL STATUS'%in% c("Critically Rare", "Rare")),]
#tspp$UID <- 1:nrow(tspp@data)

##############################################################################
### Quick plot - can be slow to ployt all points and when zoomin etc!!!
##############################################################################

mapView(x = tspp, legend = F, label = "Taxon")

##############################################################################
### Calculate EOO #and AOO - to help identify what is "narrow-range"?
##############################################################################

## Note: The IUCN set restricted range as EOO < 2000 km2 - see file:///home/jasper/Dropbox/SAEON/Projects/SANBI/ThreatenedSpecies/RedListGuidelines.pdf

## EOO using all localities irrespective of precision
eoo_all <- tspp %>% group_by(Taxon) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull() %>%
  mutate(EOO_All = st_area(.))

## EOO using only localities with precision <1km
eoo_1km <- tspp %>% filter(Precision_m < 1001) %>%
  group_by(Taxon) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull() %>%
  mutate(EOO_1km = st_area(.))

## EOO using only localities with precision <500m
eoo_500m <- tspp %>% filter(Precision_m < 501) %>%
  group_by(Taxon) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull() %>%
  mutate(EOO_500m = st_area(.))

## Join the different EOOs into one table by Taxon
eoo <- left_join(st_set_geometry(eoo_all, NULL), st_set_geometry(eoo_1km, NULL))
eoo <- left_join(eoo, st_set_geometry(eoo_500m, NULL))

rm(eoo_all, eoo_1km, eoo_500m) # Remove unneeded data objects

## Plot histograms of EOO (in log(value in square metres)): 10 = ~0.022km2 (~2Ha); 15 = 3.25km2; 20 = ~1000km2; 25 = ~75000km2
eoo %>% pivot_longer(cols = 2:4, names_to = "precision") %>%
  ggplot() +
  geom_histogram(aes(log(value))) +
  facet_wrap(~precision)

## AOO (frequency count based on a raster of 10km grid resolution)
# Get grid, crop, set unique values, extract points and summarize for 1km grid
idgrid <- rast("/home/jasper/Documents/Datasets/WilsonCloud/MODCF_monthlymean_01.tif")
idgrid <- terra::crop(idgrid, vect(tspp))     # Crop to our points
values(idgrid) <- 1:nrow(idgrid)*ncol(idgrid) # Set unique values for each cell
iddat <- terra::extract(idgrid, vect(tspp))   # Extract values for each species locality
tspp$oneKMindex <- iddat[,2]                  # Add unique cell values to locality layer
aoo1km <- tspp %>% group_by(Taxon) %>% summarise(AOO_1km = n_distinct(oneKMindex)) # Summarize number of unique cells for each species

# # Set grid resolution, extract points and summarize for 10km grid
idgrid4 <- idgrid
res(idgrid4) <- res(idgrid)*2
idgrid <- project(idgrid, idgrid4)
iddat <- terra::extract(idgrid, vect(tspp))
tspp$fourKMindex <- iddat[,2]
aoo4km <- tspp %>% group_by(Taxon) %>% summarise(AOO_4km = n_distinct(fourKMindex))

## Remove unwanted objects
rm(idgrid, idgrid4, iddat)

## Bind range stats together
aoo <- left_join(st_set_geometry(aoo1km, NULL), st_set_geometry(aoo4km, NULL))
speciesstats <- left_join(eoo, aoo)

## Plot histograms (in log(count of cells)): EN = 500km2 = log(125) = ~4.8; CE = 100km2 = log(25) = ~3.2
aoo %>% pivot_longer(cols = 2:3, names_to = "gridsize") %>%
  ggplot() +
  geom_histogram(aes(log(value))) +
  facet_wrap(~gridsize)

##############################################################################
### Get NFEPA5 data and extract to species localities
##############################################################################

## Get wetland layer
wet <- st_read("/home/jasper/Documents/Datasets/NBA FINAL DOCS/NBA FINAL DOCS mirror/NBA2018_Spatial_Datasets/Freshwater Realm/NBA2018_National_Wetland_map5/NBA2018_National Wetland Map 5 and confidence map/NBA2018_NWM5_AEA.shp")

## Transform CRS of locality data to match, buffer points by precision and convert to class "SpatVector"
tloc <- st_transform(tspp, st_crs(wet))
vloc <- tloc %>% st_buffer(dist = Precision_m) # Thos step not working?...
vloc$Locality_Area <- st_area(vloc)
vdat <- exact_extract(dat, vloc, c("median", "mean", "stdev"))

#mapview::mapview(tspp, zcol = "Taxon", quiet = T)

## Reproject locality polygons and crop wetland layer
vloc <- st_transform(tspp, st_crs(wet))
wet <- st_crop(wet, vloc)

## Extract data to polygons, calculate areas of overlap, reshape to wide table and join to points 
wint <- st_intersection(vloc, wet)
wint$Wet_Area <- st_area(wint)

wdat <- wint %>% 
  st_set_geometry(NULL) %>%
  group_by(ID) %>%
  filter(Wet_Area == max(Wet_Area, na.rm = T)) %>% # filter the data.frame to keep row where x is maximum
  dplyr::select(ID, CS_L4A, Wet_Area)

# wdat <- wint %>% dplyr::select(ID, CS_L4A, Wet_Area) %>% 
#   st_set_geometry(NULL) %>% 
#   pivot_wider(names_from = CS_L4A, values_from = Wet_Area, values_fn = sum, values_fill = 0)

vloc <- left_join(vloc, wdat, by = "ID")

## Remove unwanted objects
rm(wet, wdat, wint)
gc()

# ART: Artificial
# CVB: Channelled Valley-Bottom**
# DEPR: Depression (including the ‘Pan’ categories of the NGI)**
# EST: Estuary
# FLAT: Wetland Flat**
# FLOOD: Floodplain**
# RIVER: River (Riverine Wetland)**
# SEEP: Seep**
# UVB: Unchannelled Valley-Bottom**

##############################################################################
### Get Wilson et al 2016 mean January cloud data and extract to species localities
##############################################################################

cld <- rast("/home/jasper/Documents/Datasets/WilsonCloud/MODCF_monthlymean_01.tif")
clddat <- terra::extract(cld, vect(tspp))
# summarize by species

