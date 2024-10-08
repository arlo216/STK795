library(terra)
library(sf)
library(readxl)
library(wesanderson)


poi <- st_read('data/rawData/POIs/gis_osm_pois_a_free_1.shp') # points of interest
melusi_boundary <- st_read('data/rawData/Melusi/Melusi-polygon.shp')
bbox <- st_bbox(melusi_boundary)

# POIs
schools <- poi[poi$fclass == "school", ]
hospitals <- poi[poi$fclass == "hospital", ]
police_stations <- poi[poi$fclass == "police", ]
schools_in_melusi <- schools[st_within(schools, melusi_boundary, sparse = FALSE), ]
hospitals_in_melusi <- hospitals[st_within(hospitals, melusi_boundary, sparse = FALSE), ]
police_stations_in_melusi <- police_stations[st_within(police_stations, melusi_boundary, sparse = FALSE), ]

# raster data
MELUSI_RASTER_1 <- rast('data/processedData/raster1.tif')
MELUSI_RASTER_cropped_1 <- crop(MELUSI_RASTER_1, melusi_boundary)


MELUSI_RASTER_2 <- rast('data/processedData/raster2.tif')
MELUSI_RASTER_cropped_2 <- crop(MELUSI_RASTER_2, melusi_boundary)

MELUSI_RASTER_combined <- merge(MELUSI_RASTER_cropped_1, MELUSI_RASTER_cropped_2)
writeRaster(MELUSI_RASTER_combined, 'data/processedData/combined_raster.tif', overwrite=TRUE)

MELUSI_RASTER_combined <- rast('data/processedData/combined_raster.tif')
# data points from excel file
melusi_data <- read_excel('data/rawData/Abbridged Version.xlsx')
points <- st_as_sf(melusi_data, coords = c("Longitude", "Latitude"),crs=crs(MELUSI_RASTER))

# digital surface model
plot(MELUSI_RASTER_combined,xlim=c(bbox["xmin"], bbox["xmax"]), ylim=c(bbox["ymin"], bbox["ymax"]),
     main="Digital surface model of Melusi")

# hillshade plot 
# slope <- terrain(MELUSI_RASTER_cropped, "slope", unit="radians")
# aspect <- terrain(MELUSI_RASTER_cropped, "aspect", unit="radians")
# hill <- shade(slope, aspect, 45, 270)
slope = terrain(MELUSI_RASTER_combined,'slope', unit='degrees')
aspect = terrain(MELUSI_RASTER_combined,'aspect',unit='degrees')
hill = shade(slope,aspect,40,315)
plot(hill, col=grey(0:100/100), legend=FALSE, main="Hillshade")
plot(MELUSI_RASTER_combined, col=wes_palette("Zissou1Continuous"), add=TRUE,) #could use topo.colors rather
plot(st_geometry(points), col='#ECECEC',add=TRUE, pch=16)
plot(st_geometry(hospitals_in_melusi), col="#153243",add=TRUE,border=NA)
plot(st_geometry(police_stations_in_melusi), col="#153243",add=TRUE,border=NA)
plot(st_geometry(schools_in_melusi), col="#153243",add=TRUE,border=NA)


# # degrees instead of radians
# slope = terrain(MELUSI_RASTER_cropped,'slope', unit='degrees')
# aspect = terrain(MELUSI_RASTER_cropped,'aspect',unit='degrees')
# hill = shade(slope,aspect,40,315)
# # plot(MELUSI_RASTER_cropped,main="DEM for Medellin [meters]", col=terrain.colors(25,alpha=0.7))
# plot(slope,main="Slope (degrees)", col=topo.colors(25,alpha=0.7))
# # plot(aspect,main="Aspect for Medellin [degrees]", col=rainbow(25,alpha=0.7))
