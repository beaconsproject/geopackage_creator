## Geopackage Creator

## Introduction

The *Geopackage Creator* is an R/Shiny app that allows users to create a spatial database for use with the other BEACONs tools, principally the Disturbance Explorer and the Hydrology Explorer. The only input required by a user is a polygon layer within a geopackage file that defines a study area located within northern BC or the Yukon.

<center><img src="pics/app.jpg" width="600"></center>

## Description

The Geopackage Creator has two views:

- Mapview - view map layers
- Overview - tool description

<center><img src="pics/mapview.png" width="340"><img src="pics/overview.png" width="300"></center>

## Using the app

Using the app consists of three simple steps:

1. Select study area
2. Create geopackage
3. Save geopackage

As a final step, you should open the newly created geopackage in a gis e.g., QGIS or ArcGIS

### 1. Select study area

Click on "Select study area" to upload a boundary layer in a geopackage file (".gpkg" extension).

<center>
<img src="pics/select.png" width="600">
</center>

### 2. Create geopackage

Click on "Create geopackage" to create a geopackage file contain 6 layers:

- boundary
- fires
- linear disturbances
- areal disturbances
- intact forest landscapes (2000, 2020)

<center>
<img src="pics/create1.png" width="300">
<img src="pics/create2.png" width="300">
</center>

### 3. Save geopackage

Click on the "Save geopackage" button to save the newly created layers in a geopackage file (".gpkg" extension).

<center><img src="pics/save.png" width="600"></center>

## After saving the geopackage file...

Use QGIS or ArcGIS to check to make sure that all the layers exist in your study area.

<center><img src="pics/qgis.png" width="600"></center>
