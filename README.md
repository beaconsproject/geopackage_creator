# Geopackage Creator

January 28, 2025

This repo contains an R/Shiny app designed to assist users in creating a geopackage file required for use with the BEACONs **Disturbance Explorer** app. The app can be used to create a data package for a study area located within the Yukon and British Columbia. Three sources of data, located in the www folder, are needed to use the app: bp_datasets.gpkg, project.gpkg, and species.gpkg. The latter is very large and needs to be downloaded separately and placed in the www folder. All layers are projected to the Yukon Albers equal area coordinate reference system (CRS; EPSG:3578).

Because of the dependency on a large input dataset (bp_datasets.gpkg), the app can only be run on a local machine. The following steps need to be following to install and run the app:

1. Install R (download from [r-project.org](https://www.r-project.org/) and follow instructions)
2. Start R and install the following additional packages:

```r
install.packages(c("sf","terra","leaflet","tidyverse","shinydashboard","shinycssloaders","shinyjs","markdown","shinyMatrix"))
```

3. Download the Geopackage Creator app from GitHub
    
  - Go to https://github.com/beaconsproject/geopackage_creator
  - Click on the green "Code" button
  - Select the "Download ZIP" option
  - Unzip the downloaded file to a location on your hard drive e.g., "D:\github\beaconsproject\geopackage_creator"

4. Download the bp_datasets.gpkg from Google drive
    
- Download the [bp_datasets.gpkg](https://drive.google.com/file/d/10864Smj6nCOB12c6B2F0bjqk3VCgshJV/view?usp=drive_link) file
- Copy it to the "www" folder in app folder e.g., "D:\github\beaconsproject\geopackage_creator\www'

5. In R, set the working directory to the folder where app is located:

```r
setwd('D:/github/beaconsproject/geopackage_creator')
```

6. In R, start the Shiny app:

```r
shiny::runApp()
```
