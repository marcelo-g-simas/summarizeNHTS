library("sp")
library("rgdal")
library("rgeos")
library("maptools")
library("geojsonio")
library("ggplot2")
library("data.table")

#==============================================================================================================#

get_alaska <- function() {
  state_layer <- readOGR(dsn=shape_file_folder(),layer = 'cb_2015_us_state_20m')
  alaska_layer <- state_layer[state_layer$NAME == 'Alaska', ]
  return(alaska_layer)
}

move_hawaii_and_alaska <- function(x, column_name, alaska_regex_filter, hawaii_regex_filter) {
  # parameter x should be a SpatialPolygonsDataFrame with an Albers projection (e.g. esri:102003)
  # Alaska; AK; 02
  alaska <- x[grepl(alaska_regex_filter, x[[column_name]]), ]
  # Alaska takes more work because of its area-deflation/polygon-shrinking requirement.
  # This method merges the input Alaska with a stashed benchmark-Alaska so that the maptools elide() output is always "coordinate-matching".
  # If you pass two different spatial objects to elide() that cover the same geography, elide() will only return geography-matching outputs if their area is identical.
  # The area won't always be identical like it is for county/congressional/100% coverage areas.
  # For example, geography sub-areas like metroplitan areas will only occupy a fraction of the Alaska "parent" area.
  base_alaska <- get_alaska()
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

#==============================================================================================================#

fortify_spatial_data_frame <- function(x, id_field_name) {
  if(missing(id_field_name)) {
    id <- "id"
  } else {
    names(x)[which(names(x) == id_field_name)] <- "id"
  }
  x@data$id <- rownames(x@data)
  x.points <- fortify(x, region="id")
  x.df <- merge(x.points, x@data, by="id")
  return(x.df)
}

#==============================================================================================================#

