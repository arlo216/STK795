library(factoextra)
library(sf)
library(ggplot2)
library(wesanderson)
library(cluster)
library(sf)

set.seed(123)
coords <- st_coordinates(points)
fviz_nbclust(coords, 
             kmeans, 
             method="silhouette",
             linecolor = "#EC7A05")

# wcss <- as.data.frame(wcss)
# x=1:10
# ggplot(wcss,aes(x=x,y=wcss)) +
#   geom_line( color="grey") +
#   geom_point(shape=21, color="black", fill="#B1C492", size=6) + 
#   theme_minimal() +
#   scale_x_continuous(breaks =c(0,1,2,3,4,5,6,7,8,9,10)) +
#   ylab("Within cluster sum of squares (WCSS)") +
#   xlab("Value of k")

km <- kmeans(coords, 2)


### data frame 
coords_df <- as.data.frame(coords)
cluster_centres <- as.data.frame(km$centers)
coords_df$cluster <- as.factor(km$cluster)

### plotting
palette <- c("#EBCC2A","#3B9AB2")
ggplot(coords_df, aes(X, Y, color = cluster)) +
  geom_point() +
  geom_point(data = cluster_centres, aes(X, Y), color = "#F11B00", shape = 19, size = 4) +
  theme_minimal() +
  labs(title = "K-Means Clustering of Points") + 
  xlim(610000,614000) +
  ylim(7152000,7157000) + 
  scale_color_manual(values = palette) 

### convert cluster centres to sf and find the nodes that will represent them
cluster_centres <- st_as_sf(cluster_centres, coords=c("X","Y"), crs=32735)
nearest_nodes_clusters = st_nearest_feature(cluster_centres, connected)
#3554 1409
