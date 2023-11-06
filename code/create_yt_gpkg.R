# Create data package ('gpkg') for the disturbance_explorer app (Yukon version)
# (The terra package is required for some layers to circumvent topology errors)
# PV 2023-10-10

library(sf)
library(dplyr)
library(terra)

################################################################################
# USER-DEFINED PARAMETERS
################################################################################
# Location of Yukon geodatabase
yt_crs <- 3578 # Yukon Albers equal area (GeoYukon)
yt_gdb <- '../Curated_datasets/yt_datasets.gdb'

# Location of study area boundary (geopackage or shapefile)
study_area <- 'study_areas/LittleRancheria_FDA10AA/LittleRancheria_FDA10AA_study_area_1M_YT_BC.shp'

# Output name and crs for data package (name must start with "fda")
gpkg_out <- 'data_packages/yt_LittleRancheria_FDA10AA.gpkg'
################################################################################

# Yukon boundary - for clipping other datasets
yt <- st_read(yt_gdb, 'bnd')

# Study area boundary
bnd <- st_read(study_area) %>%
  st_transform(yt_crs) %>%
  st_intersection(yt) %>%
  st_union()
st_write(bnd, gpkg_out, 'fda', delete_layer=T)

# YG surface disturbances
sd_line <- st_read(yt_gdb, 'Surface_Disturbance_Linear_Features') %>%
  st_intersection(bnd) %>%
  st_cast('MULTILINESTRING')
st_write(sd_line, gpkg_out, 'sd_line', delete_layer=T)
sd_poly <- vect(yt_gdb, 'Surface_Disturbance_Areal_Features') %>%
  st_as_sf() %>%
  st_cast('MULTIPOLYGON') %>%
  st_intersection(bnd) %>%
  st_cast('MULTIPOLYGON')
st_write(sd_poly, gpkg_out, 'sd_poly', delete_layer=T)

# YG fires
fires <- vect(yt_gdb, 'Fire_History') %>%
  st_as_sf() %>%
  st_cast('MULTIPOLYGON') %>%
  st_intersection(bnd) %>%
  st_cast('MULTIPOLYGON')
st_write(fires, gpkg_out, 'fires', delete_layer=T)

# Intact forest landscapes 2000 and 2020
ifl_2000 <- st_read(yt_gdb, 'Global_IFL_2000') %>%
  st_intersection(bnd)
st_write(ifl_2000, gpkg_out, 'ifl_2000', delete_layer=T)
ifl_2020 <- st_read(yt_gdb, 'Global_IFL_2020') %>%
  st_intersection(bnd)
st_write(ifl_2020, gpkg_out, 'ifl_2020', delete_layer=T)

# Catchments (1:1,000,000)

# Stream network

# Quartz claims

# Placer claims

# Check that all layers exist in the right projection
st_layers(gpkg_out)
