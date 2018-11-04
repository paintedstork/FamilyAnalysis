library(maptools)
library(rgdal)
library(sp)
library(rgdal)
library(raster)
library(sp)

lists_per_family_per_grid   <- readRDS("family_frequency.rds")
species_per_family_per_grid <- readRDS("family_species.rds")

Resolution = 0.5

polygonise <- function (index, grid)
{
  x_coord = c (
    grid$LONGITUDE [index] + Resolution/2,
    grid$LONGITUDE [index] + Resolution/2,
    grid$LONGITUDE [index] - Resolution/2,
    grid$LONGITUDE [index] - Resolution/2
  )
  
  y_coord = c (
    grid$LATITUDE [index] + Resolution/2,
    grid$LATITUDE [index] - Resolution/2,
    grid$LATITUDE [index] - Resolution/2,
    grid$LATITUDE [index] + Resolution/2
  )

  return (Polygons( list (Polygon (cbind (x_coord, y_coord)),index)))
}

p_list <- mapply (polygonise, 
                  1:nrow(species_per_family_per_grid),
                  MoreArgs = list (grid = species_per_family_per_grid))

sp_list <- SpatialPolygons(p_list, 1:nrow(species_per_family_per_grid))
proj4string(sp_list)<- CRS("+proj=longlat +datum=WGS84")
sp_tras_list <-spTransform(sp_list,CRS("+proj=longlat"))

# Set Margins
par(mar=c(1,1,1,1))

rownames(species_per_family_per_grid) <- sapply(slot(sp_tras_list, "polygons"), function(x) slot(x, "ID"))
rownames(lists_per_family_per_grid) <- sapply(slot(sp_tras_list, "polygons"), function(x) slot(x, "ID"))

diversityGrids <- SpatialPolygonsDataFrame(sp_tras_list, 
                                         species_per_family_per_grid)

frequencyGrids <- SpatialPolygonsDataFrame(sp_tras_list, 
                                         lists_per_family_per_grid)

raster::shapefile(diversityGrids, "Diversity_India.shp", overwrite=TRUE)
raster::shapefile(frequencyGrids, "Frequency_India.shp", overwrite=TRUE)
