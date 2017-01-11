library(ggplot2)
library(ggiraph)
library(geojsonio)
source("map(functions).R")

load_map_layer <- function(layer) {
  
  if(layer == 'nation_layer') {
    
    nation_layer <- geojson_read("./resources/geography/cb_2015_us_nation_20m(composite).geojson", what = "sp")
    nation_layer <<- fortify_spatial_data_frame(nation_layer, "id")
    
    message('Loaded object: ',layer)

  } else if(layer == 'state_layer') {
    
    state_layer <- geojson_read("./resources/geography/cb_2015_us_state_20m(composite).geojson", what = "sp")
    state_layer <<- fortify_spatial_data_frame(state_layer, "id")

    message('Loaded object: ',layer)
    
  } else if(layer == 'cbsa_layer') {
    
    cbsa_layer <- geojson_read("./resources/geography/cb_2015_us_cbsa_20m(composite).geojson", what = "sp")
    cbsa_layer <<- fortify_spatial_data_frame(cbsa_layer, "id")

    message('Loaded object: ',layer)
  
  } else {
    
    warning('No known layer name: ', dQuote(layer), '.')
    return(NULL)
    
  }
  
}
