#======================================================================================================#
shape_file_folder <- function() {'prep/geography/shp'}

# Takes census cartographic boundary shapefile link, downloads, and unzips to shp folder
get_cb_shp_file <- function(zip_link) {
  temp_shp_zip <- tempfile()
  cat('Downloading Shape file:\n')
  download.file(zip_link, temp_shp_zip)
  file_path <- file.path(getwd(), shape_file_folder())
  cat('Unzipping and copying to:', file_path,'\n')
  unzip(temp_shp_zip, junkpaths = TRUE, exdir = file_path)
  cat('Removing Temporary file.\n')
  unlink(temp_shp_zip)
  cat('Complete!')
}


#======================================================================================================#
# If shape file does not exist:
# Download nation layer
if ( length(list.files(shape_file_folder(), pattern = 'cb_2015_us_nation_20m')) == 0 ) {
  get_cb_shp_file('http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_nation_20m.zip')
}
# Download state layer
if ( length(list.files(shape_file_folder(), pattern = 'cb_2015_us_state_20m')) == 0 ) {
  get_cb_shp_file('http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_20m.zip')
}
# Download csa layer
if ( length(list.files(shape_file_folder(), pattern = 'cb_2015_us_cbsa_20m')) == 0 ) {
  get_cb_shp_file('http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_cbsa_20m.zip')
}
# Download csa layer
if ( length(list.files(shape_file_folder(), pattern = 'cb_2015_us_csa_20m')) == 0 ) {
  get_cb_shp_file('http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_csa_20m.zip')
}
# Download csa layer
if ( length(list.files(shape_file_folder(), pattern = 'cb_2015_us_county_20m')) == 0 ) {
  get_cb_shp_file('http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_20m.zip')
}
# Download time zone layer
# if ( length(list.files('prep/geography/shp', pattern = 'tz_us')) == 0 ) {
#   get_cb_shp_file('http://efele.net/maps/tz/us/tz_us.zip')
# }

