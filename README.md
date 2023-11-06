# Geopackage Creator

November 6, 2023

This repo contains an R/Shiny app to assist users in creating a geopackage file required for use with the BEACONs **Disturbance Explorer** app. The app can be used to create a data package for a study area located within the Yukon and British Columbia. Only one source of data, located in the www folder, is needed to use the app: bp_datasets.gpkg. All data layers within a geopackage should be in the same coordinate reference system (CRS). Currently, EPSG 3578 (Yukon Albers) is the only CRS used. Some R scripts are also provided in the code folder for more advanced users.

The app is located at: https://beaconsproject.shinyapps.io/geopackage_creator
