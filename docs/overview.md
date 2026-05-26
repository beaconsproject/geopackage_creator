## Welcome to BEACONs GeoPackage Creator

<br>

This repo contains an R/Shiny application designed to assist users in creating a geopackage file required for use with the BEACONs Disturbance Explorer app. 
The app can be used to create a data package for a study area located within the Yukon and British Columbia. Output are projected to the Yukon Albers equal area 
coordinate reference system (CRS; EPSG:3578).

The **Dataset Requirements** tab details spatial layers required by the application, including their data sources.

<br>

### Functionality
    
The app consists of three sections:
<br>
<br>
#### Select study area

The underlying datasets are hosted in the cloud. Users only need to upload a study area or area of interest (AOI), either as a GeoPackage (.gpkg) 
with a selected layer or as a shapefile. The latter is then used to clip all required layers and generate the output. 

By default, processed layers are displayed on the interactive map for visualization and validation purposes. For large datasets or study areas, rendering 
layers in Leaflet can slow down the application. The **Enable interactive map (slower for large study areas)** option can be unchecked to disable map 
rendering and improve processing speed.

📌 Note: The study area must capture the full extent of disturbance layers to ensure accurate analysis.

<br>

#### Select spatial layers

This section allows users to configure the layers included in the analysis. All required base layers are selected by default and are necessary for 
processing. Users can optionally include additional layers such as Placer Claims, Quartz Claims, and Caribou Herds using checkboxes. A fire-year range 
selector is also available to filter and include only fires occurring within the specified time period.

<br>

#### Download GeoPackage
    
Download selected layers in a GeoPackage intersecting the uploaded study area or area of interest. 
  