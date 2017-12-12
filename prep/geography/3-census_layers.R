#======================================================================================================#
# State and Nation
state_layer <- readOGR(dsn=shape_file_folder(),layer = 'cb_2015_us_state_20m')
state_layer <- spTransform(state_layer,CRS("+init=epsg:2163"))
state_layer <- state_layer[!grepl("PR", state_layer$STUSPS), ]
state_layer <- move_hawaii_and_alaska(state_layer, "STATEFP", "02", "15")
#======#
nation_layer <- unionSpatialPolygons(state_layer, rep('01', length(state_layer)))
df <- data.frame(NAME = 'United States', row.names = '01')
nation_layer <- SpatialPolygonsDataFrame(nation_layer, df)
nation_layer <- fortify_spatial_data_frame(nation_layer, "id")
setDT(nation_layer)
nation_layer <- nation_layer[, list(GEOID = id, NAME, group, long, lat)]
#======#
state_layer <- fortify_spatial_data_frame(state_layer, "id")
setDT(state_layer)
state_layer <- state_layer[, list(GEOID, NAME, group, long, lat)]

# CBSA
cbsa_layer <- readOGR(dsn=shape_file_folder(),layer="cb_2015_us_cbsa_20m")
cbsa_layer <- spTransform(cbsa_layer,CRS("+init=epsg:2163"))
cbsa_layer <- cbsa_layer[!grepl(", PR", cbsa_layer$NAME), ]
cbsa_layer <- move_hawaii_and_alaska(cbsa_layer, "NAME", ", AK", ", HI")
cbsa_layer <- fortify_spatial_data_frame(cbsa_layer, "id")
setDT(cbsa_layer)
cbsa_layer <- cbsa_layer[, list(GEOID, NAME, group, long, lat)]

# CSA
csa_layer <- readOGR(dsn=shape_file_folder(),layer="cb_2015_us_csa_20m")
csa_layer <- spTransform(csa_layer,CRS("+init=epsg:2163"))
csa_layer <- csa_layer[!grepl(", PR", csa_layer$NAME), ]
csa_layer <- fortify_spatial_data_frame(csa_layer, "id")
setDT(csa_layer)
csa_layer <- csa_layer[, list(GEOID, NAME, group, long, lat)]

# County
county_layer <- readOGR(dsn=shape_file_folder(),layer = 'cb_2015_us_county_20m')
county_layer <- spTransform(county_layer,CRS("+init=epsg:2163"))
county_layer <- county_layer[!grepl("72", county_layer$STATEFP), ]
county_layer <- move_hawaii_and_alaska(county_layer, "STATEFP", "02", "15")
county_layer <- fortify_spatial_data_frame(county_layer, "id")
setDT(county_layer)
county_layer <- county_layer[, list(GEOID, NAME, group, long, lat)]

#======================================================================================================#
# Census Divisions 
new_england <- c("ME","NH","VT","CT","MA","RI")
middle_atlantic <- c("NY","NJ","PA")
east_north_central <- c("IL","IN","MI","OH","WI")
west_north_central <- c("IA","KS","MO","MN","ND","NE","SD")
south_atlantic <- c("DC","DE","FL","GA","MD","NC","SC","WV","VA")
east_south_central <- c("AL","KY","MS","TN")
west_south_central <- c("AR","LA","OK","TX")
mountain <- c("AZ","CO","ID","MT","NM","NV","UT","WY")
pacific <- c("AK","CA","HI","OR","WA")

# Read and prep state shapefile
state_layer_sp <- readOGR(dsn=shape_file_folder(),layer = 'cb_2015_us_state_20m')
state_layer_sp <- spTransform(state_layer_sp,CRS("+init=epsg:2163"))
state_layer_sp <- state_layer_sp[!grepl("PR", state_layer_sp$STUSPS), ]
state_layer_sp <- move_hawaii_and_alaska(state_layer_sp, "STATEFP", "02", "15")

# Function to return census division subsets
create_census_division <- function(state_layer, id, name, state_list) {
  cd <- state_layer[state_layer$STUSPS %in% state_list,]
  cd <- unionSpatialPolygons(cd, rep(id, length(cd) ))
  df <- data.frame(NAME = name, row.names = id)
  cd <- SpatialPolygonsDataFrame(cd, df)
  return(cd)
}

# assemble divisions
census_division_layer <- do.call(rbind,
  list(
    create_census_division(state_layer_sp, '01', 'New England', new_england),
    create_census_division(state_layer_sp, '02', 'Middle Atlantic', middle_atlantic),
    create_census_division(state_layer_sp, '03', 'East North Central', east_north_central),
    create_census_division(state_layer_sp, '04', 'West North Central', west_north_central),
    create_census_division(state_layer_sp, '05', 'South Atlantic', south_atlantic),
    create_census_division(state_layer_sp, '06', 'East South Central', east_south_central),
    create_census_division(state_layer_sp, '07', 'West South Central', west_south_central),
    create_census_division(state_layer_sp, '08', 'Mountain', mountain),
    create_census_division(state_layer_sp, '09', 'Pacific', pacific)
  )
)

#======================================================================================================#
# Census Regions
northeast <- c('01','02')
midwest <- c('03','04')
south <- c('05','06','07')
west <- c('08','09')

# Function to return census region subsets
create_census_region <- function(census_division_layer, id, name, region_list) {
  cr <- census_division_layer[row.names(census_division_layer) %in% region_list,]
  cr <- unionSpatialPolygons(cr, rep(id, length(cr) ))
  df <- data.frame(NAME = name, row.names = id)
  cr <- SpatialPolygonsDataFrame(cr, df)
  return(cr)
}

# assemble regions
census_region_layer <- do.call(rbind,
  list(
    create_census_region(census_division_layer, '01', 'Northeast', northeast),
    create_census_region(census_division_layer, '02', 'Midwest', midwest),
    create_census_region(census_division_layer, '03', 'South', south),
    create_census_region(census_division_layer, '04', 'West', west)
  )
)

#======================================================================================================#

# Fortify Census Region Layer
census_region_layer <- fortify_spatial_data_frame(census_region_layer, "id")
setDT(census_region_layer)
census_region_layer <- census_region_layer[, list(GEOID = id, NAME, group, long, lat)]

# Fortify Census Division Layer
census_division_layer <- fortify_spatial_data_frame(census_division_layer, "id")
setDT(census_division_layer)
census_division_layer <- census_division_layer[, list(GEOID = id, NAME, group, long, lat)]


#======================================================================================================#
# State Tile Layer
library(tidyverse)
library(tilemapr)
# Convert tile coordinates to SpatialPolygonsDataFrame
usa <- square_usa(style="Wall Street Journal")
usa_list <- usa[, c("long", "lat", "group")]
usa_list <- split(usa_list, usa$group) # create each polygon by splitting the data frame by spatial group identifier
usa_list <- lapply(usa_list, function(x) x[(names(x) %in% c("long", "lat"))]) # Polygon() only accepts 2 column numeric matrix of x/y
polygons <- lapply(usa_list, Polygon)
polygons <- lapply(seq_along(polygons), function(i) Polygons(list(polygons[[i]]), ID = names(usa_list)[i])) # add back spatial group identifier
spatial_polygons <- SpatialPolygons(polygons, proj4string = CRS("+proj=longlat +datum=WGS84")) # dummy/filler CRS
state_tile_layer <- SpatialPolygonsDataFrame(spatial_polygons,
  data.frame(
    group = unique(usa$group),
    state = unique(usa$region_abr),
    NAME = tools::toTitleCase(unique(usa$region)),
    row.names = unique(usa$group))
)
state_tile_layer <- merge(state_tile_layer, unique(state_layer[, list(GEOID, NAME)]), by = 'NAME')
state_tile_layer <- fortify_spatial_data_frame(state_tile_layer, "id")
setDT(state_tile_layer)
state_tile_layer <- state_tile_layer[, list(GEOID, NAME, group = group.y, long, lat)]

#======================================================================================================#
# Check the specs of each layer file

layer_names <- c('nation_layer','state_layer', 'state_tile_layer', 'cbsa_layer','csa_layer',
                 'county_layer','census_region_layer','census_division_layer')

for(x in layer_names) {
  cat('=========================\n')
  cat(x,':\n', sep = '')
  cat(class(get(x)),'\n')
  cat(colnames(get(x)),'\n')
  print(object.size(get(x)), units = 'MB')
}

#======================================================================================================#
