---
format: md
---

## Select study area

The underlying datasets are hosted in the cloud. Users only need to upload a study area or area of interest (AOI), either as a GeoPackage (.gpkg) 
with a selected layer or as a shapefile. The latter is then used to clip all required layers and generate the output. 

By default, processed layers are displayed on the interactive map for visualization and validation purposes. For large datasets or study areas, rendering 
layers in Leaflet can slow down the application. The **Enable interactive map (slower for large study areas)** option can be unchecked to disable map 
rendering and improve processing speed.

📌 Note: The study area must capture the full extent of disturbance layers to ensure accurate analysis.
