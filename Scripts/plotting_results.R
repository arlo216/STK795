police_station_pairs <- data.frame(
  from = c(3554, 3554, 3554, 1409, 1409, 1409),
  to = c(4024, 3871, 429, 4024, 3871, 429)
)

hospital_pairs <- data.frame(
  from = c(1409),
  to = c(8272)
)

school_pairs <- data.frame(
  from = c(3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 3554, 
           1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 
           1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409),
  to = c(8613, 6611, 7298, 7637, 5208, 8359, 790, 7757, 8073, 7135, 4204, 6484, 514, 3727, 
         7948, 8613, 6611, 5474, 7324, 6163, 8350, 6940, 4681, 3343, 7298, 7637, 5208, 8551, 6955, 
         2519, 7778, 8359, 790, 7757, 8073, 7135, 4204, 6484, 514, 3727, 1131, 2812, 7660, 3733)
)



predefined_colors <- c(
  "#8E24AA", "#2B7A92", "#4CAF50", "#D81B60", "#6A1B9A", "#E53935", "#FF5733", "#D50032", "#F57F20", "#0E6251",
  "#3EBB8C", "#008CBA", "#1976D2", "#5C6BC0", "#FF5722", "#FF9800", "#7F8C8D", "#2ECC71", "#2E86C1", "#8BC34A",
  "#009688", "#00BCD4", "#2196F3", "#673AB7", "#E91E63", "#9C27B0", "#F44336", "#B39DDB", "#00E676", "#1F618D",
  "#FF4081", "#7C4DFF", "#1E88E5", "#FF8A65", "#4E342E", "#388E3C", "#34495E", "#4DD0E1", "#C2185B", "#BF360C",
  "#5D4037", "#F06292", "#FFB74D", "#0097A7", "#D32F2F"
)


# Function to plot the built-in paths
plot_builtin_paths <- function(net, from, to, color) {
  builtin_paths <- st_network_paths(net, from = from, to = to, weights = "weight")
  
  builtin_paths %>%
    slice(1) %>%
    pull(node_paths) %>%
    unlist()
  
  builtin_paths %>%
    pull(node_paths) %>%
    walk(~ plot_path(.x, color))
  
}

# Function to plot the custom weighted paths
plot_weighted_paths <- function(net, adj_list, from, to, color) {
  weighted_paths <- weighted_dijkstra(adj_list, startNode = from, endNode = to)
  plot_path(as.numeric(weighted_paths), color = color)
}

# Function to plot a single path
plot_path <- function(node_path, color) {
  net %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(col = color, cex = 1, lwd = 1, add = TRUE)
}

# Plot all paths on one figure
plot(net, col = "grey")
plot(net %>% activate("edges"), col = "grey", cex = 0.5, add = TRUE)
for (i in 1:nrow(school_pairs)) {
  from <- school_pairs$from[i]
  to <- school_pairs$to[i]
  # plot_builtin_paths(net, from, to, predefined_colors[i])
  # plot_weighted_paths(net,adj_list,from,to,predefined_colors[i])
  plot_builtin_paths(net, from, to, "#EC7A05")
  plot_weighted_paths(net,adj_list,from,to,"#3B9AB2")
  
  net %>%
    activate("nodes") %>%
    filter(row_number() == from) %>%
    plot(col = "red", pch = 8, cex = 2, lwd = 2, add = TRUE)
  
  net %>%
    activate("nodes") %>%
    filter(row_number() == to) %>%
    plot(col = "#ffe32b", pch = 8, cex = 2, lwd = 2, add = TRUE)
}

