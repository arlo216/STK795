library(dplyr)
library(tibble)
library(sfnetworks)
library(sf)
library(tidygraph)
library(igraph)
library(purrr)
library(TSP)
library(raster)

#### initialise the network
net = as_sfnetwork(net, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length())
###

### elevation
dem <- raster('Data/processedData/combined_raster.tif')

# Update the sfnetwork with the new node attributes
sfnetwork_object <- activate(net, "nodes") %>%
  mutate(node_id = row_number())
nodes <- st_as_sf(sfnetwork_object, 'nodes')
nodes$elevation <- extract(dem, nodes)

sfnetwork_object <- activate(sfnetwork_object, "nodes") %>%
  mutate(elevation = nodes$elevation)

edges <- st_as_sf(sfnetwork_object, 'edges')

# Add node elevations to edges
edges <- edges %>%
  mutate(
    from_elevation = nodes$elevation[match(edges$from, nodes$node_id)],
    to_elevation = nodes$elevation[match(edges$to, nodes$node_id)],
    elevation_change = to_elevation - from_elevation
  )

net <- activate(sfnetwork_object, "edges") %>%
  mutate(
    from_elevation = edges$from_elevation,
    to_elevation = edges$to_elevation,
    elevation_change = edges$elevation_change
  )

#### prep the edges

# select the edges, convert to sf object to keep the linestring attributes and then convert to tibble
edges_tbl <- net %>%
  activate("edges") %>%
  st_as_sf() %>%
  as_tibble()

# swap every 'from' and 'to' edge to make every road segment bidirectional
reverse_edges_tbl <- edges_tbl %>%
  mutate(temp = from) %>%  
  mutate(from = to, to = temp) %>%  
  dplyr::select(-temp)

# combine the original edges with the reversed edges
full_edges <- bind_rows(edges_tbl, reverse_edges_tbl)
####

#### precompute an adjacency list
create_adjacency_list <- function(edges_tbl) {
  adj_list <- edges_tbl %>%
    mutate(weight = as.numeric(weight),elevation_change = as.numeric(elevation_change)) %>% 
    dplyr::select(from, to, weight, elevation_change) %>%
    group_by(from) %>%
    summarize(neighbors = list(tibble(to = to, weight = weight, elevation_change = elevation_change)), .groups = 'drop')
  
  # Convert to a named list for faster access
  adj_list <- adj_list %>% 
    deframe()
  
  return(adj_list)
}

adj_list <- create_adjacency_list(full_edges)
####

#### calculate elevation penalty
elevation_penalty <- function(elev,dist){
  return((abs(elev)/dist)*100)
  # return(0)
}

# Step 2: Implement Dijkstra's algorithm using the adjacency list and priority queue
weighted_dijkstra <- function(adj_list, startNode, endNode) {
  # Initialize distance table with infinity and 0 for the start node
  distances <- rep(Inf, max(as.numeric(names(adj_list))))
  names(distances) <- 1:max(as.numeric(names(adj_list)))
  distances[startNode] <- 0
  
  # Priority queue 
  pq <- tibble(node = startNode, distance = 0)
  # Track the shortest path
  previous <- vector("list", max(as.numeric(names(adj_list))))
  
  while(nrow(pq) > 0) {
    # get node with smallest distance
    current <- pq %>% filter(distance == min(distance)) %>% slice(1)
    pq <- pq %>% filter(node != current$node)
    
    currentNode <- current$node
    currentDist <- current$distance
    
    # check: if end node is reached
    if (currentNode == endNode) {
      # Reconstruct path
      path <- c()
      while (!is.null(previous[[currentNode]])) {
        path <- c(currentNode, path)
        currentNode <- previous[[currentNode]]
      }
      return(c(startNode, path))
    }
    
    # check neighbors
    neighbors <- adj_list[[as.character(currentNode)]]
    if (!is.null(neighbors)) {
      for (i in 1:nrow(neighbors)) {
        neighbor <- neighbors$to[i]
        elevation_penalty <- elevation_penalty(neighbors$elevation_change[i],neighbors$weight[i])
        alt <- currentDist + neighbors$weight[i] + elevation_penalty
        
        if (alt < distances[neighbor]) {
          distances[neighbor] <- alt
          previous[[neighbor]] <- currentNode
          pq <- add_row(pq, node = neighbor, distance = alt)
        }
      }
    }
  }
  # If no path was found
  return(NULL)
}

# Usage example
shortest_path <- weighted_dijkstra(adj_list, startNode = 3554, endNode = 5015)
print(shortest_path)
