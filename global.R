required_packages <- c("sf", "terra", "tidyverse", "leaflet", "shinydashboard", "shinycssloaders",
                       "shinyjs", "markdown", "shiny"
)

options(shiny.maxRequestSize=500*1024^2) 

# Install missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the packages
#invisible(lapply(required_packages, library, character.only = TRUE))

library(shiny)
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


# read_shp_from_upload: read a shapefile from fileInput
read_shp_from_upload <- function(upload_input) {
  req(upload_input)
  required_extensions <- c("shp", "shx", "dbf", "prj")
  infile <- upload_input
  file_extensions <- tools::file_ext(infile$name)
  if (all(required_extensions %in% file_extensions)) {
    dir <- unique(dirname(infile$datapath))
    outfiles <- file.path(dir, infile$name)
    name <- tools::file_path_sans_ext(infile$name[1])
    purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y))
    shp_path <- file.path(dir, paste0(name, ".shp"))
    if (file.exists(shp_path)) {
      #return(sf::st_read(shp_path))
      shp <- sf::st_read(shp_path)
      assign(name, shp)
      return(shp)
    } else {
      showModal(modalDialog(
        title = "Shapefile (.shp) is missing.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
  } else {
    showModal(modalDialog(
      title = "Extension file is missing",
      "Please upload all necessary files for the shapefile (.shp, .shx, .dbf and .prj).",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return()
  }
}