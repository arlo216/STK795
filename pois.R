library(purrr)
library(RColorBrewer)
library(dplyr)
library(writexl) 

### get POI centres
# schools
sc_cs <-  st_centroid(schools_in_melusi)
plot(schools_in_melusi$geometry)
plot(sc_cs$geometry,add=TRUE,col="blue")
ggplot() + 
  geom_sf(data = schools_in_melusi, fill = 'white') +
  geom_sf(data = sc_cs, color = 'red') 

# hospitals
hs_cs <-  st_centroid(hospitals_in_melusi)
plot(hospitals_in_melusi$geometry)
plot(hs_cs$geometry,add=TRUE,col="blue")
ggplot() + 
  geom_sf(data = hospitals_in_melusi, fill = 'white') +
  geom_sf(data = hs_cs, color = 'red') 

# police stations
ps_cs <-  st_centroid(police_stations_in_melusi)
plot(police_stations_in_melusi$geometry)
plot(ps_cs$geometry,add=TRUE,col="blue")
ggplot() + 
  geom_sf(data = police_stations_in_melusi, fill = 'white') +
  geom_sf(data = ps_cs, color = 'red') 

nearest_nodes_schools = st_nearest_feature(sc_cs, connected)
nearest_nodes_hospitals = st_nearest_feature(hs_cs, connected)
nearest_nodes_police = st_nearest_feature(ps_cs, connected)

# builtin_paths = st_network_paths(net, from = 1409, to = 8727, weights = "weight")
# weighted_paths = weighted_dijkstra(adj_list, startNode = 1409, endNode = 8727)

### plot the built-in shortest path
# builtin_paths %>%
#   slice(1) %>%
#   pull(node_paths) %>%
#   unlist()
# 
# plot(net, col = "grey")
# plot(net %>% activate("edges"), col = "grey", cex=0.5)
# 
# builtin_paths %>%
#   pull(node_paths) %>%
#   walk(plot_path)
# ### 
# 
# plot_path <- function(node_path) {
#   color <- "#D81B60"
#   net %>%
#     activate("nodes") %>%
#     slice(node_path) %>%
#     plot(col = color, cex = 1, lwd = 1, add = TRUE)
#   
# }
# plot_path(as.numeric(weighted_paths)) # when calling custom dijkstra
# net %>%
#   activate("nodes") %>%
#   filter(row_number() == 1409) %>%
#   plot(col = "red", pch = 8, cex = 3, lwd = 3, add = TRUE)
# 
# net %>%
#   activate("nodes") %>%
#   filter(row_number() == 8727) %>%
#   plot(col = "#EC7A05", pch = 8, cex = 3, lwd = 3, add = TRUE)

### ALL SHORTEST PATHS
pois_shortest_paths <- function(features) {
  all_paths <- list() 
  
  for (cluster in nearest_nodes_clusters) {
    print(paste("Processing cluster:", cluster))
    cluster_paths <- list()  
    
    for (feature in features) {
      print(paste("Checking feature:", feature))
      sp_1 <- st_network_paths(net, from = cluster, to = feature, weights = "weight")
      sp_2 <- weighted_dijkstra(adj_list, startNode = cluster, endNode = feature)
      
      # Store this path and distance in the current cluster's paths list
      cluster_paths[[as.character(feature)]] <- list(
        path_1 = sp_1 %>% slice(1) %>% pull(node_paths) %>% unlist(),  # Store the path
        path_2 = sp_2
      )
    }
    
    # Store all paths for this cluster
    all_paths[[as.character(cluster)]] <- cluster_paths
  }
  return(all_paths)
}
all_shortest_paths_hospitals <- pois_shortest_paths(nearest_nodes_hospitals)
all_shortest_paths_police <- pois_shortest_paths(nearest_nodes_police)
all_shortest_paths_schools <- pois_shortest_paths(nearest_nodes_schools)

find_non_identical_paths <- function(all_paths) {
  non_identical_paths <- list()
  
  for (cluster in names(all_paths)) {
    cluster_paths <- all_paths[[cluster]]
    different_paths <- list()
    
    for (feature in names(cluster_paths)) {
      paths <- cluster_paths[[feature]]
      path_1 <- paths$path_1
      path_2 <- paths$path_2
      
      # Check if paths are not identical
      if (!identical(path_1, path_2)) {
        different_paths[[feature]] <- paths  # Store only non-identical paths
      }
    }
    
    # Only store clusters with differing paths
    if (length(different_paths) > 0) {
      non_identical_paths[[cluster]] <- different_paths
    }
  }
  
  return(non_identical_paths)
}
non_identical_paths <- find_non_identical_paths(all_shortest_paths_hospitals)






















