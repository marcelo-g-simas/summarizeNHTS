source('prep/geography/1-get_shape_files.R')
source('prep/geography/2-geography_functions.R')
source('prep/geography/3-census_layers.R')

setattr(nation_layer, 'layer_name', deparse(substitute(nation_layer)))
setattr(state_layer, 'layer_name', deparse(substitute(state_layer)))
setattr(state_tile_layer, 'layer_name', deparse(substitute(state_tile_layer)))
setattr(cbsa_layer, 'layer_name', deparse(substitute(cbsa_layer)))
setattr(census_region_layer, 'layer_name', deparse(substitute(census_region_layer)))
setattr(census_division_layer, 'layer_name', deparse(substitute(census_division_layer)))
setattr(csa_layer, 'layer_name', deparse(substitute(csa_layer)))
setattr(county_layer, 'layer_name', deparse(substitute(county_layer)))

class_name <- 'HTS.geography.layer'
class(nation_layer) <- c(class(nation_layer), class_name)
class(state_layer) <- c(class(state_layer), class_name)
class(state_tile_layer) <- c(class(state_layer), class_name)
class(cbsa_layer) <- c(class(cbsa_layer), class_name)
class(census_region_layer) <- c(class(census_region_layer), class_name)
class(census_division_layer) <- c(class(census_division_layer), class_name)
class(csa_layer) <- c(class(csa_layer), class_name)
class(county_layer) <- c(class(county_layer), class_name)

# Write layers to package data folder
devtools::use_data(
  nation_layer,
  state_layer,
  state_tile_layer,
  cbsa_layer,
  # csa_layer,
  county_layer,
  census_region_layer,
  census_division_layer,
  overwrite = TRUE
)

