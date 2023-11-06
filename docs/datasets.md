## Datasets
  
The purpose of the Geopackage Creator is to create a geopackage file (".gpkg") containing six map layers for use with the Disturbance Explorer and Hydrology Explorer apps. Given a user-uploaded polygon, the app will each map layers from a larger database ("bp_datasets.gpkg") and save them in a geopackage file. The layers and their attributes are described below in the event that a user would like to create a similar dataset i) using different data sources or ii) for a location outside of the current geographic limits of the apps' database.
  
### Map layers

All layers within the geopackage should be in the same coordinate reference system. Currently, this is limited to the Yukon Albers equal area projection (EPSG:3578) but will change in the near future to allow any projected coordinate reference system (CRS).
    
  - fda : a single polygon outlining the boundary of the study area e.g., an FDA; displayed as "Study area" in the map legend.
  - ifl_2000 : distribution of intact forest landscapes in the year 2000; displayed as "Intactness 2000" in the map legend.
  - ifl_2020 : distribution of intact forest landscapes in the year 2020; displayed as "Intactness 2020" in the map legend.
  - fires : distribution of wildfire polygons for the past 70 years; displayed as "Fires" in the map legend.
  - sd_line : linear anthropogenic surface disturbance features; displayed as "Linear disturbances" in the map legend.
  - sd_poly : areal (polygonal) anthropogenic surface disturbance features; displayed as "Areal disturbances" in the map legend.

#### Attributes of linear and areal disturbances

The fda, ifl_2000, ifl_2020, and fires do not have any specific requirements for attributes; however, they must all be polygonal layers. Conversely, the linear and areal disturbance layers require the following minimum set of attributes (additional attributes will be ignored):

The **Linear_Features** layer must include the following attributes:
    
  - TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
  - TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Survey / Cutline, Access Road
  
The **Areal_Features** layer must include the following attributes:
    
  - TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
  - TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Drill Pad, Clearing

Additional attributes may be added in the near future.
