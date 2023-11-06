# Create study area boundary (geopackage or shapefile)
# It can be an FDA, and catchments-modified FDA, or a user-defined polygons
# In the third case, all intersecting catchments will be selected
# PV 2023-10-06

library(sf)
library(dplyr)
library(terra)

################################################################################
# USER-DEFINED PARAMETERS
################################################################################
# Location of Yukon geodatabase
yt_gdb <- 'C:/Users/PIVER37/Documents/gisdata/123/yt_datasets.gdb'

# Set working directory to location of study area boundary
#setwd('data_tmp')

# Use catchments to modify boundary?
bnd_type = "fda-catch" # "fda", "fda-catch", "user-defined"
fda_code = '10AB'

# Location of study area boundary (geopackage or shapefile)
gpkg_in <- 'code/fda10ad_bnd.gpkg'

# Output name and crs for data package (name must start with "fda")
gpkg_out <- 'www/fda10ad.gpkg'
yt_crs <- 3578 # Yukon Albers equal area (GeoYukon)
################################################################################

# Yukon boundary - for clipping other datasets
yt <- st_read(yt_gdb, 'Mining_Districts_50k_dslv')

# Study area boundary
if (use_catch==TRUE) {
bnd <- st_read(yt_gdb, 'Catchments') %>%
  filter(FDA==fda_code) %>%
  st_union
} else {
  bnd <- st_read(gpkg_in) %>%
    st_transform(yt_crs) %>%
    st_intersection(yt)
}
st_write(bnd, gpkg_out, 'fda', delete_layer=T)

