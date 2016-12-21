library(ggplot2)
library(ggiraph)
library(geojsonio)
source("map(functions).R")

household <- read.csv("data/2009/HHV2PUB.CSV", stringsAsFactors = FALSE)

nation_layer <- geojson_read("./resources/geography/cb_2015_us_nation_20m(composite).geojson", what = "sp")
nation_layer <- fortify_spatial_data_frame(nation_layer, "id")

state_layer <- geojson_read("./resources/geography/cb_2015_us_state_20m(composite).geojson", what = "sp")
state_layer <- fortify_spatial_data_frame(state_layer, "id")

cbsa_layer <- geojson_read("./resources/geography/cb_2015_us_cbsa_20m(composite).geojson", what = "sp")
cbsa_layer <- fortify_spatial_data_frame(cbsa_layer, "id")


############################################################
#                                                          #
#                       CBSA Example                       #
#                                                          #
############################################################
geom_nation_layer <- geom_polygon(
	data = nation_layer,
	mapping = aes(
		x = long,
		y = lat,
		group = group),
	fill = "#DDDDDD")

geom_state_layer <- geom_polygon(
	data = state_layer,
	mapping = aes(
		x = long,
		y = lat,
		group = group),
	fill = NA,
	color = "#FFFFFF",
	size = 0.35)

geom_cbsa_layer <- geom_polygon_interactive(
	data = cbsa_layer,
	mapping = aes(
		x = long,
		y = lat,
		group = group,
		tooltip = paste0(gsub("'", "", NAME), "<br>", AWATER),
		fill = AWATER,
		use_jquery = TRUE,
		data_id = CBSAFP,
		hover_css = "fill:orange"),
	alpha = 1/2)

map <- ggplot()
map <- map + geom_nation_layer
map <- map + geom_cbsa_layer
map <- map + geom_state_layer
map <- map + coord_fixed()
map <- map + theme_void()
map <- map + scale_fill_gradient(low = "#f2f0f7", high = "#54278f")
map_widget <- ggiraph(code = {print(map)}, zoom_max = 12)
map_widget



############################################################
#                                                          #
#                      STATE Example                       #
#                                                          #
############################################################
geom_nation_layer <- geom_polygon(
	data = nation_layer,
	mapping = aes(
		x = long,
		y = lat,
		group = group),
	fill = "#DDDDDD")

geom_state_layer <- geom_polygon_interactive(
	data = state_layer,
	mapping = aes(
		x = long,
		y = lat,
		group = group,
		tooltip = paste0(gsub("'", "", NAME), "<br>", AWATER),
		fill = AWATER,
		use_jquery = TRUE,
		data_id = STATEFP,
		hover_css = "fill:orange"),
	color = "#FFFFFF",
	size = 0.35)

map <- ggplot()
map <- map + geom_nation_layer
map <- map + geom_state_layer
map <- map + coord_fixed()
map <- map + theme_void()
map <- map + scale_fill_gradient(low = "#f2f0f7", high = "#54278f")
map_widget <- ggiraph(code = {print(map)}, zoom_max = 12)
map_widget
