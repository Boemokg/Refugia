# PARALLLEL INTERSECTION OF LARGE LAYERS
library(sf)
library(mapview)
library(s2)
library(tictoc)
library(parallel)
library(doParallel)

# a download: https://data.statistik.gv.at/data/OGDEXT_DSR_1_STATISTIK_AUSTRIA_20111031.zip
# b download: https://data.statistik.gv.at/data/OGDEXT_GEM_1_STATISTIK_AUSTRIA_20220101.zip

# read data
a <- read_sf('C:/Users/NHLKAG001/Downloads/STATISTIK_AUSTRIA_DSR_20111031.shp')
b <- read_sf('C:/Users/NHLKAG001/Downloads/STATISTIK_AUSTRIA_GEM_20220101.shp')

# Check layers
# mapview(a)+b

tic()
# Correct topology (this is just a recommendation)
# This can take a while but it worth
a <- st_make_valid(st_cast(a, "POLYGON")) # transform into POLYGONS
b <- st_make_valid(b)
toc()

tic()
# Process in parallel (intersection by "village" (name) in b)
# Proceso de cálculo de para cada TIPO (MUNI, EEO, ESF, EEO_ESF)
cl = parallel::makeCluster(detectCores(), type="FORK")
doParallel::registerDoParallel(cl, detectCores())

# Union in parallel
names <- unique(b$name) #different munis
pols = foreach(i=1:length(names)) %dopar% {
  x <- b %>% dplyr::filter(name == names[[i]]) %>% st_make_valid()
  crop <- st_crop(a, x) %>% st_make_valid()
  
  # remove invalid polygons
  # this allows the process to continue. Sometimes topology is not perfectly 
  # made and tinny pols can ruin your process
  notvalid <- which(s2_is_valid_detail(crop)==FALSE)
  if(length(notvalid) > 0){crop <- crop[-notvalid,]}
  
  # intersection between x and eeoval and esfval
  x2 <- crop %>% st_intersection(x) %>% st_make_valid()
  
  # "dissolve" to get
  x3 <- x2 %>% group_by(NAME, id, name) %>% 
    summarize()
  
  # return
  x3
}

parallel::stopCluster(cl)
toc()

cast <- lapply(pols, function(x) st_cast(x, "MULTIPOLYGON"))

# merge all
# Merge polygons
ab <- sf::st_as_sf(data.table::rbindlist(cast)) # superfast
ab <- ab %>% rename(NAME_A = NAME, 
                    NAME_B = name,
                    ID = id)
mapview(ab)

# export
st_write(ab, "ab.gpkg", append = FALSE)
