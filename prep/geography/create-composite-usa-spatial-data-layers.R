library("rgdal")
library("rgeos")
library("maptools")
library("geojsonio")

move_hawaii_and_alaska <- function(x, column_name, alaska_regex_filter, hawaii_regex_filter) {
	# parameter x should be a SpatialPolygonsDataFrame with an Albers projection (e.g. esri:102003)
	# Alaska; AK; 02
	alaska <- x[grepl(alaska_regex_filter, x[[column_name]]), ]
	# Alaska takes more work because of its area-deflation/polygon-shrinking requirement.
	# This method merges the input Alaska with a stashed benchmark-Alaska so that the maptools elide() output is always "coordinate-matching".
	# If you pass two different spatial objects to elide() that cover the same geography, elide() will only return geography-matching outputs if their area is identical.
	# The area won't always be identical like it is for county/congressional/100% coverage areas.
	# For example, geography sub-areas like metroplitan areas will only occupy a fraction of the Alaska "parent" area.
	base_alaska <- geojson_read("cb_2015_us_state_alaska_20m.geojson", what = "sp")
	base_alaska <- spTransform(base_alaska,CRS(proj4string(x)))
	alaska_stashed <- alaska
	alaska <- rbind( # Any references to GEOID in this function will cause failure if the column name doens't exist in the input. Should probably just try rbinding by first column and just force dummy names to match
		subset(alaska, select = "GEOID"),
		subset(base_alaska,	select = "GEOID"),
		makeUniqueIDs = TRUE)
	alaska <- elide(alaska, rotate=-50)
	alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.5)
	alaska <- elide(alaska, shift=c(-2100000, -2500000)) # wgs84: c(-120, 20) #
	# remove benchmarking alaska from object and drop the only remaining data frame column and recreate SpatialPolygonsDataFrame using original stashed data
	alaska <- alaska[1:length(alaska)-length(base_alaska), ]
	alaska$GEOID <- NULL
	alaska <- SpatialPolygonsDataFrame(alaska, as.data.frame(alaska_stashed))
	proj4string(alaska) <- proj4string(x)
	# Hawaii; HI; 15
	hawaii <- x[grepl(hawaii_regex_filter, x[[column_name]]), ]
	hawaii <- elide(hawaii, rotate=-35)
	hawaii <- elide(hawaii, shift=c(5400000, -1400000)) # wgs84: c(40, 0)
	proj4string(hawaii) <- proj4string(x)
	# Pull old and push new
	x <- x[!(grepl(alaska_regex_filter, x[[column_name]]) | grepl(hawaii_regex_filter, x[[column_name]])), ]
	x <- rbind(x, alaska, hawaii, makeUniqueIDs = TRUE)
	return(x)
}


state_layer <- readOGR(dsn="cb_2015_us_state_20m",layer="cb_2015_us_state_20m")
state_layer <- spTransform(state_layer,CRS("+init=epsg:2163"))
state_layer <- state_layer[!grepl("PR", state_layer$STUSPS), ]
state_layer <- move_hawaii_and_alaska(state_layer, "STATEFP", "02", "15")
geojson_write(state_layer, lat = 'lat', lon = 'long', file = "cb_2015_us_state_20m(composite).geojson")

nation_layer <- gUnaryUnion(state_layer)
geojson_write(nation_layer, lat = 'lat', lon = 'long', file = "cb_2015_us_nation_20m(composite).geojson")

cbsa_layer <- readOGR(dsn="cb_2015_us_cbsa_20m",layer="cb_2015_us_cbsa_20m")
cbsa_layer <- spTransform(cbsa_layer,CRS("+init=epsg:2163"))
cbsa_layer <- cbsa_layer[!grepl(", PR", cbsa_layer$NAME), ]
cbsa_layer <- move_hawaii_and_alaska(cbsa_layer, "NAME", ", AK", ", HI")
geojson_write(cbsa_layer, lat = 'lat', lon = 'long', file = "cb_2015_us_cbsa_20m(composite).geojson")

