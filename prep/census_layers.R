library(summarizeNHTS)
library(data.table)

#======================================================================================================#

# Census Regions 
new_england <- c("ME","NH","VT","CT","MA","RI")
middle_atlantic <- c("NY","NJ","PA")
east_north_central <- c("IL","IN","MI","OH","WI")
west_north_central <- c("IA","KS","MO","MN","ND","NE","SD")
south_atlantic <- c("DC","DE","FL","GA","MD","NC","SC","WV","VA")
east_south_central <- c("AL","KY","MS","TN")
west_south_central <- c("AR","LA","OK","TX")
mountain <- c("AZ","CO","ID","MT","NM","NV","UT","WY")
pacific <- c("AK","CA","HI","OR","WA")

# Define Census Region layer
census_region_layer <- state_layer
setDT(census_region_layer)

# Remove stale columns
census_region_layer$NAME <- NULL
census_region_layer$id <- NULL
census_region_layer$order <- NULL
census_region_layer$hole <- NULL
census_region_layer$piece <- NULL
census_region_layer$group <- NULL
census_region_layer$GEOID <- NULL
census_region_layer$STATEFP <- NULL
census_region_layer$STATENS <- NULL
census_region_layer$AFFGEOID <- NULL
census_region_layer$LSAD <- NULL
census_region_layer$ALAND <- NULL
census_region_layer$AWATER <- NULL

# Assign appriopriate labels by state groups
census_region_layer[STUSPS %in% new_england, c('NAME','id') := list('New England','01')]
census_region_layer[STUSPS %in% middle_atlantic, c('NAME','id') := list('Middle Atlantic','02')]
census_region_layer[STUSPS %in% east_north_central, c('NAME','id') := list('East North Central','03')]
census_region_layer[STUSPS %in% west_north_central, c('NAME','id') := list('West North Central','04')]
census_region_layer[STUSPS %in% south_atlantic, c('NAME','id') := list('South Atlantic','05')]
census_region_layer[STUSPS %in% east_south_central, c('NAME','id') := list('East South Central','06')]
census_region_layer[STUSPS %in% west_south_central, c('NAME','id') := list('West South Central','07')]
census_region_layer[STUSPS %in% mountain, c('NAME','id') := list('Mountain','08')]
census_region_layer[STUSPS %in% pacific, c('NAME','id') := list('Pacific','09')]
census_region_layer[, NAME := as.factor(NAME)]
census_region_layer$STUSPS <- NULL

#======================================================================================================#

# Census Divisions
northeast <- c('01','02')
midwest <- c('03','04')
south <- c('05','06','07')
west <- c('08','09')

# Define Census Division layer
census_division_layer <- census_region_layer

# Assign appriopriate labels by census regions
census_division_layer[id %in% northeast, c('NAME','id') := list('Northeast','01')]
census_division_layer[id %in% midwest, c('NAME','id') := list('Midwest','02')]
census_division_layer[id %in% south, c('NAME','id') := list('South','03')]
census_division_layer[id %in% west, c('NAME','id') := list('West','04')]



