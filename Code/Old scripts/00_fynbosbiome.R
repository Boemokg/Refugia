library("tidyverse")
library("sf")
library(viridis)

### Get veg types, trim extra columns and remove self-intersections
fynbos <- st_read("fynbos.gpkg") %>%
  dplyr::select(Name_18, BIOREGION_) %>%
  st_make_valid()

### Select only "Fynbos" vegetation types (i.e. drop Renosterveld or Strandveld) and dissolve into one big polygon
fynveg <- fynbos %>%
  filter(str_detect(Name_18, "Fynbos")) %>% # Selects only Veg Type names that incluse "Fynbos"
  summarize() #Dissolves polygons 

ggplot() +
     geom_sf(data = fynveg, aes(fill = ""))

### Get QDS grid ###
QDS_Mask <- sf::st_read("qds_pentads.geojson") %>% 
  filter(province %in% c("Western Cape","Eastern Cape", "Northern Cape")) %>%
  st_transform(st_crs(fynveg)) %>%
  group_by(qdgc) %>%
  summarize()

### Mask QDS to Fynbos Biome ###
#QDS_Mask <- st_intersection(QDS_Mask, fynveg)

### Plot QDS over Fynbos Biome ###
ggplot() +
 # geom_sf(data = fynveg, aes(fill = "")) +
  geom_sf(data = QDS_Mask, fill = NA, lwd = 0.2)


############################################################################
# ### Select only "Fynbos" bioregions (i.e. drop Renosterveld or Strandveld)
# bioregions <- fynbos %>%
#   filter(str_detect(BIOREGION_, "Fynbos")) %>%
#   mutate(Bioregion = fct_drop(BIOREGION_)) %>%
#   group_by(Bioregion) %>%
#   summarize() %>%
#   mutate(Bioregion = as.character(Bioregion))
# 
# ggplot() +
#   geom_sf(data = bioregions, aes(fill = Bioregion, color = Bioregion)) + # + #, colour = as.numeric(as.factor(Name_18)))) +
#   scale_fill_viridis(discrete = T) +
#   scale_color_viridis(discrete = T)
# #  geom_sf_text(data = stas, aes(label =shortname)) # Extra code if you want to plot text
