## Datasets
  
This page describes the required and optional map layers that are used by the Geopackage Creator and Disturbance Explorer apps.
  
### Map layers

#### Required

- **Study area** : A single polygon outlining the boundary of the study area e.g., a watershed or ecoregion or any other user-defined area.
- **Linear disturbances** : Linear anthropogenic surface disturbance features. Available from: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/
- **Areal disturbances** : Areal (polygonal) anthropogenic surface disturbance features. Available from: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/
- Fires : Distribution of wildfire polygons for the past 70 years. Available from: https://cwfis.cfs.nrcan.gc.ca/datamart
- **Intactness 2000** : Distribution of intact forest landscapes in the year 2000. Available from: https://intactforests.org
- **Intactness 2020** : Distribution of intact forest landscapes in the year 2020. Available from: https://intactforests.org
- **Protected areas** : Distribution of protected areas from the Canadian Protected and Conserved Areas Database (CPCAD). Available from: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c

#### Optional - projected

- **Quartz Claims** : Active quartz mining claims. Available from: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- **Place Claims** : Active placer mining claims. Available from: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/

#### Optional - miscellaneous

- **Caribou Herds** : Yukon caribou herd boundaries.
- **Thinhorn Sheep** : Distribution of Thinhorn sheep habitat.
- **Key Wetlands 2011** : Distribution of key wetland areas for the year 2011.

### Attributes of linear and areal disturbances

The **Linear disturbances** and **Areal disturbances** layers include the following two attributes which have to be present to use the Disturbance Explorer and Hydrology Explorer apps (additional attributes will be ignored):

The **Linear_Features** layer must include the following attributes:
    
- TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
- TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Survey / Cutline, Access Road
  
The **Areal_Features** layer must include the following attributes:
    
- TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
- TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Drill Pad, Clearing
