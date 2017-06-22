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

nation_layer <- geojson_read("../resources/geography/cb_2015_us_nation_20m(composite).geojson", what = "sp")
nation_layer <- fortify_spatial_data_frame(nation_layer, "id")

state_layer <- geojson_read("../resources/geography/cb_2015_us_state_20m(composite).geojson", what = "sp")
state_layer <- fortify_spatial_data_frame(state_layer, "id")

cbsa_layer <- geojson_read("../resources/geography/cb_2015_us_cbsa_20m(composite).geojson", what = "sp")
cbsa_layer <- fortify_spatial_data_frame(cbsa_layer, "id")


use_data(nation_layer, state_layer, cbsa_layer)
