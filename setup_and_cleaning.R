library(sfnetworks)
library(sf)
library(tidygraph)
library(igraph)
library(dbscan)
library(ggplot2)
library(readxl)

### read raw data
poi <- st_read('data/rawData/POIs/gis_osm_pois_a_free_1.shp') # points of interest
melusi_boundary <- st_read('data/rawData/Melusi N4/Melusi_N4-polygon.shp') # melusi polygon
subset_roads <- st_read('data/processedData/Melusi N4 roads/Melusi N4 roads.shp') # melusi roads

### transform to correct coordinate set
poi <- st_transform(poi, 32735)
melusi_boundary <- st_transform(melusi_boundary, 32735)
subset_roads <- st_transform(subset_roads, 32735)

### cast roads to linestring and create net 
subset_roads_linestring <- st_cast(subset_roads, "LINESTRING")
net <- as_sfnetwork(subset_roads_linestring,directed=FALSE)

# Filter POIs to get schools, hospitals, police stations
schools <- poi[poi$fclass == "school",]
hospitals <- poi[poi$fclass == "hospital", ]
police_stations <- poi[poi$fclass == "police", ]
schools_in_melusi <- schools[st_within(schools, melusi_boundary, sparse = FALSE), ]
hospitals_in_melusi <- hospitals[st_within(hospitals, melusi_boundary, sparse = FALSE), ]
police_stations_in_melusi <- police_stations[st_within(police_stations, melusi_boundary, sparse = FALSE), ]

# # POIs combined
# sch_hosp_pol <-  poi[poi$fclass == "school" | poi$fclass == "hospital" | poi$fclass == "police",]
# melusi_pois <- sch_hosp_pol[st_within(sch_hosp_pol,melusi_boundary,sparse=FALSE),]
# melusi_pois_linestring <- st_cast(melusi_pois, "MULTILINESTRING")

# data points from excel file
melusi_data <- read_excel('data/rawData/Abbridged Version.xlsx')
points <- st_as_sf(melusi_data, coords = c("Longitude", "Latitude"), crs = 4326)
points <- st_transform(points, 32735)
bbox <- st_bbox(melusi_boundary)

# print(unique(st_geometry_type(subset_roads)))

### cleaning
st_crs(net)

# make it a simple graph, but make sure to keep the shortest path
# In graph theory, a simple graph is defined as a graph that does not 
# contain multiple edges nor loop edges.
simple = net %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

subdivision = convert(simple, to_spatial_subdivision) %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

smoothed = convert(subdivision, to_spatial_smooth) %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

connected = smoothed %>%
  activate("nodes") %>%
  filter(group_components() == 1)


### final network - use 'net' throughout from here
net <- connected

### adding weights
net <- net %>% 
  activate("edges") %>%
  mutate(weight = edge_length())