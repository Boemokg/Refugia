library(sf)

qds <- st_read("https://api.birdmap.africa/sabap2/v2/pentads/project/sabap2?format=geoJSON")

st_write(qds, "/home/jasper/Documents/Datasets/QDS/qds_pentads.geojson")
