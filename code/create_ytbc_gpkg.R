# Create data package ('gpkg') for the disturbance_explorer app (Yukon+BC version)
# (The terra package is required for some layers to circumvent topology errors)
# PV 2023-10-10

library(sf)
library(dplyr)
library(terra)

################################################################################
# USER-DEFINED PARAMETERS
################################################################################
# Location of Yukon geodatabase
crs <- 3578 # Yukon Albers equal area (GeoYukon)
yt_gdb <- 'C:/Users/PIVER37/Documents/gisdata/123/yt_datasets.gdb'
bc_gdb <- 'C:/Users/PIVER37/Documents/gisdata/123/bc_datasets.gdb'
ca_gdb <- 'C:/Users/PIVER37/Documents/gisdata/123/ca_datasets.gdb'
yt_gdb <- 'C:/Users/PIVER37/Documents/gisdata/123/yt_datasets.gdb'
bc_gdb <- 'C:/Users/PIVER37/Documents/gisdata/123/bc_datasets.gdb'

# Location of study area boundary (geopackage or shapefile)
#study_area <- 'study_areas/LittleRancheria_large_bnd.gpkg'
study_area <- 'C:/Users/PIVER37/Documents/gisdata/123/ytbc_datasets.gdb'

# Output name and crs for data package
#gpkg_out <- 'data_packages/LittleRancheria_large.gpkg'
gpkg_out <- 'data_output/ytbc_datasets.gpkg'
################################################################################

# Study area boundary
bnd <- st_read(study_area, 'bnd') %>%
  st_transform(crs) %>%
  st_union()
st_write(bnd, gpkg_out, 'fda', delete_layer=T)

# YG surface disturbances
yt_line <- st_read(yt_gdb, 'Surface_Disturbance_Linear_Features') %>%
  st_intersection(bnd) %>%
  st_cast('MULTILINESTRING') %>%
  select(TYPE_INDUSTRY, TYPE_DISTURBANCE)
#st_write(yt_line, gpkg_out, 'yt_line', delete_layer=T)
yt_poly <- vect(yt_gdb, 'Surface_Disturbance_Areal_Features') %>%
  st_as_sf() %>%
  st_cast('MULTIPOLYGON') %>%
  st_intersection(bnd) %>%
  st_cast('MULTIPOLYGON') %>%
  select(TYPE_INDUSTRY, TYPE_DISTURBANCE)
#st_write(yt_poly, gpkg_out, 'yt_poly', delete_layer=T)

# BC surface disturbances
bc_line <- st_read(bc_gdb, 'Roads') %>%
  st_transform(crs) %>%
  st_intersection(bnd) %>%
  st_cast('MULTILINESTRING') %>%
  select(CAPTURE_DATE, TRANSPORT_LINE_TYPE_CODE) %>%
  mutate(TYPE_INDUSTRY='Transportation',
    TYPE_DISTURBANCE = 'Access Road') %>%
  select(TYPE_INDUSTRY, TYPE_DISTURBANCE)
#st_write(bc_line, gpkg_out, 'bc_line', delete_layer=T)

bc_poly <- vect(bc_gdb, 'cef_2021') %>%
  st_as_sf() %>%
  st_transform(crs) %>%
  st_cast('MULTIPOLYGON') %>%
  st_intersection(bnd) %>%
  st_cast('MULTIPOLYGON') %>%
  select(CEF_DISTURB_GROUP_RANK) %>%
  filter(CEF_DISTURB_GROUP_RANK <= 10) %>%
  mutate(
    TYPE_INDUSTRY = case_when(
      CEF_DISTURB_GROUP_RANK==1 ~ 'Mining',
      CEF_DISTURB_GROUP_RANK==3 ~ 'Oil and Gas',
      CEF_DISTURB_GROUP_RANK==4 ~ 'Unknown',
      CEF_DISTURB_GROUP_RANK==5 ~ 'Unknown',
      CEF_DISTURB_GROUP_RANK==6 ~ 'Urban',
      CEF_DISTURB_GROUP_RANK==9 ~ 'Forestry'),
    TYPE_DISTURBANCE = case_when(
      CEF_DISTURB_GROUP_RANK==1 ~ 'Mining',
      CEF_DISTURB_GROUP_RANK==3 ~ 'Well Pad',
      CEF_DISTURB_GROUP_RANK==4 ~ 'Uknown',
      CEF_DISTURB_GROUP_RANK==5 ~ 'Uknown',
      CEF_DISTURB_GROUP_RANK==6 ~ 'Urban',
      CEF_DISTURB_GROUP_RANK==9 ~ 'Forestry')) %>%
  select(TYPE_INDUSTRY, TYPE_DISTURBANCE)
#st_write(bc_poly, gpkg_out, 'bc_poly', delete_layer=T)

st_geometry(yt_line) <- "geometry"
st_geometry(bc_line) <- "geometry"
sd_line = rbind(yt_line, bc_line)
st_write(sd_line, gpkg_out, 'sd_line', delete_layer=T)
sd_poly = rbind(yt_poly, bc_poly)
st_write(sd_poly, gpkg_out, 'sd_poly', delete_layer=T)

# YG fires
fires <- st_read(ca_gdb, 'Fire_History') %>%
  #st_as_sf() %>%
  #st_cast('MULTIPOLYGON') %>%
  st_transform(crs) %>%
  st_intersection(bnd) %>%
  st_cast('MULTIPOLYGON')
st_write(fires, gpkg_out, 'fires', delete_layer=T)

# Intact forest landscapes 2000 and 2020
ifl_2000 <- st_read(ca_gdb, 'Global_IFL_2000') %>%
  st_transform(crs) %>%
  st_intersection(bnd)
st_write(ifl_2000, gpkg_out, 'ifl_2000', delete_layer=T)
ifl_2020 <- st_read(ca_gdb, 'Global_IFL_2020') %>%
  st_transform(crs) %>%
  st_intersection(bnd)
st_write(ifl_2020, gpkg_out, 'ifl_2020', delete_layer=T)

# Catchments (1:1,000,000)

# Stream network

# Quartz claims

# Placer claims

# Check that all layers exist in the right projection
st_layers(gpkg_out)
