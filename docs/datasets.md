## Datasets
  
This page outlines the required and optional spatial layers used by **BEACONs GeoPackage Creator**. The required layers are uploaded via a single GeoPackage. Optional layers are uploaded in the same Geopackage with the exception of **Other disturbances** (see below).

📌 For the app to recognize the spatial layers in the GeoPackage, the layer names **must exactly match the expected names shown below.** 

📌 All spatial layers must have the same projection. 

📌 For accurate analysis, the study area must contain the full extent of disturbance layers.

<br> 

### Map layers 
<br>

#### Required - GeoPackage

- **studyarea**: A polygon of the study area e.g., watershed, ecoregion or any other user-defined area.
- At least one of the following disturbance layers:
  - **linear_disturbance**: Linear human surface disturbance features e.g., roads, seismic line. 
  - **areal_disturbance**: Areal (polygonal) human surface disturbance features e.g., mine, town site, cutblock. 
  - **fires** : Distribution of wildfire polygons. The layer must have the following two attributes: (1) "YEAR" - an integer  which is the ignition year of the fire e.g., 1995 and (2) "CAUSE" - cause of ignition of the fire which can either be 'Human', 'Lightning' or 'Unknown'.  
<br>

#### Optional - GeoPackage 

Bold text is the required name of the spatial layer in the GeoPackage.

- **protected_areas**: Distribution of protected areas e.g., Canadian Protected and Conserved Areas Database. 
- **Quartz_Claims**: Quartz mining claims for hard rock mining. 
- **Placer_Claims**: Placer mining claims for the mining of waterways (e.g., stream beds, wetlands) for mineral deposits (e.g., gold).
- **Mining_Claims**: Generic option for mining claims.
- **Intact_FL_2000**: Distribution of intact forest landscapes in the year 2000 (Potapov et al. 2017).
- **Intact_FL_2020**: Distribution of intact forest landscapes in the year 2020 (Potapov et al. 2017).
<br>

#### Optional - Shapefile

There are no requirements for the shapefile name.

- **Other disturbances** (linear and areal) can be uploaded as shapefiles and included in the analysis. However, only an overall buffer size can be applied to additional disturbances.
<br>

### Demo Dataset

The demo dataset contains the following spatial layers: 

- studyarea 
- streams: https://map-data.service.yukon.ca/GeoYukon/Base/Watercourses_1M/
- linear_disturbance: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Linear_Features/
- areal_disturbance: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Areal_Features/
- Quartz_Claims: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- Placer_Claims: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/
- fires: https://cwfis.cfs.nrcan.gc.ca/datamart
- protected_areas: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- Intact_FL_2000: https://intactforests.org
- Intact_FL_2020:  https://intactforests.org
<br><br>

### References

Potapov, P., Hansen, M. C., Laestadius L., Turubanova S., Yaroshenko A., Thies C., Smith W., Zhuravleva I., Komarova A., Minnemeyer S., Esipova E. The last frontiers of wilderness: Tracking loss of intact forest landscapes from 2000 to 2013. Science Advances, 2017; 3:e1600821
