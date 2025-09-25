library(sf)
library(terra)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(markdown)

options(shiny.maxRequestSize=100*1024^2) 

bp <- 'www/bp_datasets.gpkg'
spp <- 'www/species.gpkg'
prj <- 'www/projected.gpkg'
limits <- st_read(bp, 'bnd') %>% st_transform(4326)
