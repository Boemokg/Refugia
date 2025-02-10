library(tidyverse)
library(terra)
library(sf)
library(exactextractr)

qdsS_ADU <- readRDS("Data/qds.RDS") %>%
  select(qdgc) %>%
  group_by(qdgc) %>%
 # summarise() %>%
 # st_combine() #%>%
  #st_union()

#qdsS <- st_read("/Users/jasper/Documents/Datasets/QDGC/qdgc_south africa.gpkg", layer = "tbl_qdgc_01")

# Make raster version of QDS grid
qdsR <- rast(nrows=100, ncols=88, extent = ext(qdsS))

# Get NFEPA6 data
nfepa6 <- st_read("/Users/jasper/Documents/Datasets/SANBI/NWM6_Beta_V3_20230714_CFR/NWM6_Beta_V3_20230714_CFR.shp")

# Excluse estuaries
nfp6 <- nfepa6 %>% filter(HGM_type != "Estuary") %>% summarise()

# Calculate "wetland" coverage of each QDS 
hmm <- coverage_fraction(qdsR, nfp6, crop = T)

plot(hmm[[1]])
plot(qdsS[,1], add = T, alpha = 0.5)


# Extract wetland coverage to centroids of pentads and bind to QDS/pentad layer
c_qdsS <- centroids(qdsS)

c_qdsS$WetlandCoverage <- extract(hmm[[1]], c_qdsS, ID = FALSE)

qdsS$WetlandCoverage <- pentW$lyr.1

#plot_sf(qdsS['WetlandCoverage'])


hmmS <- st_intersects(st_as_sf(as.points(hmm[[1]])), qdsS)

hmmD <- data.frame(qdgc = qdsS$qdgc[unlist(hmmS)], wetlands = st_as_sf(as.points(hmm[[1]]))$lyr.1)

c_qdsS <- st_centroid(qdsS)
