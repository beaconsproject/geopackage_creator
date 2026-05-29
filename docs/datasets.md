## Datasets
  
This page outlines the required and optional spatial layers used by **BEACONs GeoPackage Creator**. All layers, except for the user-provided study area (AOI), are hosted in the cloud and automatically accessed by the application during processing.

📌 For accurate analysis, the study area must contain the full extent of disturbance layers.

<br> 

### Available layers 
<br>

Bold text is the core layers selected by default in the GeoPackage.

- **linear_disturbances**: Linear human surface disturbance features e.g., roads, seismic line. Source: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Linear_Features/
- **areal_disturbances**: Areal (polygonal) human surface disturbance features e.g., mine, town site, cutblock. Source: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Areal_Features/
- **fires_poly**: Distribution of wildfire polygons. The layer must have the following two attributes: (1) "YEAR" - an integer  which is the ignition year of the fire e.g., 1995 and (2) "CAUSE" - cause of ignition of the fire which can either be 'Human', 'Lightning' or 'Unknown'.  Source: https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/

Note: This link points to the current public dataset. The application uses a snapshot downloaded on 2026-01-28. If the metadata on the source page shows a more recent update date (e.g., 2027-12-12), the structure may be identical but the underlying data may differ, and results may not fully match those produced in this application.

- **fires_area_composite**: Distribution of wildfire polygons. The layer must have the following two attributes: (1) "YEAR" - an integer  which is the ignition year of the fire e.g., 1995 and (2) "CAUSE" - cause of ignition of the fire which can either be 'Human', 'Lightning' or 'Unknown'.  Source: https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/
- **protected_areas**: Distribution of protected areas e.g., Canadian Protected and Conserved Areas Database. Source: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- **intact_fl_2000**: Distribution of intact forest landscapes in the year 2000 (Potapov et al. 2017). Source: https://intactforests.org
- **intact_fl_2020**: Distribution of intact forest landscapes in the year 2020 (Potapov et al. 2017). Source: https://intactforests.org
- quartz_claims: Quartz mining claims for hard rock mining.  Source: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- placer_claims: Placer mining claims for the mining of waterways (e.g., stream beds, wetlands) for mineral deposits (e.g., gold). Source: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/
- caribou_herds: Caribou herd annual distribution (YG),
<br>


### References

Potapov, P., Hansen, M. C., Laestadius L., Turubanova S., Yaroshenko A., Thies C., Smith W., Zhuravleva I., Komarova A., Minnemeyer S., Esipova E. The last frontiers of wilderness: Tracking loss of intact forest landscapes from 2000 to 2013. Science Advances, 2017; 3:e1600821
