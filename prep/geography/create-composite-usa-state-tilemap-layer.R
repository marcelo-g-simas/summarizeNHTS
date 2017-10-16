#################################################################################
# Preview already put-together usa state tile layouts using package "tilemapr"
library(tidyverse)
library(tilemapr)

style <- "Wall Street Journal"
#"NPR", "The New York Times", "538", "Propublica", "Bloomberg", "The Guardian", "Wall Street Journal", "WNYC", "The Marshall Project"

g <- ggplot()
g <- g + geom_polygon(data=square_usa(style=style), aes(x=long, y=lat, group=group))
g <- g + geom_text(data=square_usa(style=style, center = TRUE), aes(x=long, y=lat, label=region_abr), color="#ffffff")
g
#################################################################################


#################################################################################
# Convert tile coordinates to SpatialPolygonsDataFrame
usa <- square_usa(style=style)
usa_list <- usa[, c("long", "lat", "group")]
usa_list <- split(usa_list, usa$group) # create each polygon by splitting the data frame by spatial group identifier
usa_list <- lapply(usa_list, function(x) x[(names(x) %in% c("long", "lat"))]) # Polygon() only accepts 2 column numeric matrix of x/y
library(sp)
polygons <- lapply(usa_list, Polygon)
polygons <- lapply(seq_along(polygons), function(i) Polygons(list(polygons[[i]]), ID = names(usa_list)[i])) # add back spatial group identifier
spatial_polygons <- SpatialPolygons(polygons, proj4string = CRS("+proj=longlat +datum=WGS84")) # dummy/filler CRS
spatial_polys_df <- SpatialPolygonsDataFrame(spatial_polygons,
	data.frame(
		group = unique(usa$group),
		state = unique(usa$region_abr),
		NAME = toupper(unique(usa$region)),
		row.names = unique(usa$group))
	)


library(geojsonio)
geojson_write(spatial_polys_df, lat = 'lat', lon = 'long', file = "us_state_tile_layer.geojson")
#################################################################################