library(ggplot2)
library(rgeos)
library(maptools)
library(geojsonio)

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

nation_layer <- geojson_read("prep/geography/cb_2015_us_nation_20m(composite).geojson", what = "sp")
nation_layer <- fortify_spatial_data_frame(nation_layer, "id")

state_layer <- geojson_read("prep/geography/cb_2015_us_state_20m(composite).geojson", what = "sp")
state_layer <- fortify_spatial_data_frame(state_layer, "id")

cbsa_layer <- geojson_read("prep/geography/cb_2015_us_cbsa_20m(composite).geojson", what = "sp")
cbsa_layer <- fortify_spatial_data_frame(cbsa_layer, "id")

state_tile_layer <- geojson_read("prep/geography/us_state_tile_layer.geojson", what = "sp")
state_tile_layer <- fortify_spatial_data_frame(state_tile_layer, "id")
# make names more consistent with census state layer names
names(state_tile_layer)[names(state_tile_layer) == "group.x"] <- "group"
state_tile_layer$group.y <- NULL
names(state_tile_layer)[names(state_tile_layer) == "state"] <- "STUSPS"
state_tile_layer$NAME <- tools::toTitleCase(tolower(state_tile_layer$NAME))
# add fips code to be consistent with census state layer
fips <- maps::state.fips
fips <- fips[, c("fips","abb")]
fips <- unique(fips)
state_tile_layer$row_order <- 1:nrow(state_tile_layer)
state_tile_layer <- merge(state_tile_layer, fips, all.x = TRUE, by.x="STUSPS", by.y="abb")
state_tile_layer <- state_tile_layer[order(state_tile_layer$row_order), ]
state_tile_layer$row_order <- NULL
state_tile_layer[state_tile_layer$STUSPS=="AK", "fips"] <- "2"
state_tile_layer[state_tile_layer$STUSPS=="HI", "fips"] <- "15"
state_tile_layer$fips <- ifelse(state_tile_layer$fips %in% c("1","2","4","5","6","8","9"), paste0("0",state_tile_layer$fips), state_tile_layer$fips)
names(state_tile_layer)[names(state_tile_layer) == "fips"] <- "STATEFP"


devtools::use_data(nation_layer, state_layer, state_tile_layer, cbsa_layer)
