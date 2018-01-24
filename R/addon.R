#' @import data.table
#' @importFrom rgdal readOGR
#' @import sp

#' @export
prepare_add_on_files <- function(directory, weight_type = '5day') {
  
  capitalize_header <- function(data_file) {
    csv_lines <- readLines(data_file)
    csv_lines[1] <- toupper(csv_lines[1])
    # Remove 5day flag if weight type is 5day
    if (weight_type == '5day' & grepl('weights', data_file)) {
      csv_lines[1] <- gsub('5D', '', csv_lines[1])
    }
    writeLines(csv_lines, data_file)
    cat(dQuote(data_file), 'complete. \n')
  }
  
  data_files <- list.files(directory, full.names = T)
  
  if (weight_type == '5day') {
    file.rename(data_files, gsub('_5day','', data_files))
    file.remove(data_files[grepl('_7day',data_files)])
  } else if (weight_type == '7day') {
    file.rename(data_files, gsub('_7day','', data_files))
    file.remove(data_files[grepl('_5day',data_files)])
  } else {
    stop('Invalid weight type.')
  }
  
  data_files <- list.files(directory, full.names = T)
  
  invisible(sapply(data_files, capitalize_header))
}

#prepare_add_on_files('GDOT/csv/2017', '5day')

#============================================================================================#

get_loc_variables <- function(data_file, var) {
  label <- switch(gsub('HH_|WRK_|SCHL_|TRP_','',var),
                  CBSA = 'OMB Core Based Statistical Area',
                  STATEFIPS = 'Census State FIPS Code',
                  CNTYFIPS = 'Census County FIPS Code'
  )
  data.table(NAME = var, TABLE = data_file, TYPE = 'character', LABEL = label)
}


get_loc_values <- function(var) {
  geo_layer <- switch(gsub('HH_|WRK_|SCHL_|TRP_','',var),
                      CBSA = cbsa_layer,
                      STATEFIPS = state_layer,
                      CNTYFIPS = county_layer
  )
  rbind(
    unique(geo_layer[order(GEOID), list(NAME = var, VALUE = as.character(GEOID), LABEL = as.character(NAME))]),
    data.table(NAME = var, VALUE = '-1', LABEL = 'Appropriate skip')
  )
  
}


na2neg1 <- function(x) {
  x_class <- class(x)
  ifelse(is.na(x), as(-1, x_class), x)
}


#============================================================================================#
# HOUSEHOLD

append_household_data <- function(data, location, cb) {
  data_file <- 'household'
  join_key <- get_table_keys(data_file)
  loc_cols <- 'CNTYFIPS'
  new_loc_cols <- paste0('HH_', loc_cols)
  
  if(any(new_loc_cols %in% colnames(data$data$household))) {
    warning(paste(new_loc_cols, collapse = ', '),' already exists in the household dataset.')
    return()
  }
  
  # Merge location data
  data$data$household <- merge(
    x = data$data$household, 
    y = location[LOCNO == 100, c(join_key, loc_cols), with = F],
    all.x = T
  )
  
  # Create "Table-specific" location name
  setnames(data$data$household, loc_cols, new_loc_cols)
  
  # Replace NA values with "-1"
  data$data$household[, (new_loc_cols) := lapply(.SD, na2neg1), .SDcols = new_loc_cols]
  
  # Get variables and values
  loc_variables <- rbindlist(lapply(new_loc_cols, get_loc_variables, data_file = data_file))
  loc_values <- rbindlist(lapply(new_loc_cols, get_loc_values))
  
  # Append variable and value lookups to codebook 
  cb$variables <- rbind(cb$variables, loc_variables)
  cb$values <- rbind(cb$values, loc_values)
  rm(loc_variables, loc_values)
  
  invisible(gc())
  cat('Appended household location data.\n')
  
}

#============================================================================================#
# Person Work

append_person_work_data <- function(data, location, cb) {
  data_file <- 'person'
  join_key <- get_table_keys(data_file)
  loc_cols <- c('STATEFIPS','CNTYFIPS','CBSA')
  new_loc_cols <- paste0('WRK_', loc_cols)
  
  if(any(new_loc_cols %in% colnames(data$data$person))) {
    warning(paste(new_loc_cols, collapse = ', '),' already exist in the person dataset.')
    return()
  }
  
  # Merge location data
  data$data$person <- merge(
    x = data$data$person, 
    y = location[LOCTYPE == '02', c(join_key, loc_cols), with = F],
    all.x = T
  )
  
  # Create "Table-specific" location name
  setnames(data$data$person, loc_cols, new_loc_cols)
  
  # Replace NA values with "-1"
  data$data$person[, (new_loc_cols) := lapply(.SD, na2neg1), .SDcols = new_loc_cols]
  
  # Get variables and values
  loc_variables <- rbindlist(lapply(new_loc_cols, get_loc_variables, data_file = data_file))
  loc_values <- rbindlist(lapply(new_loc_cols, get_loc_values))
  
  # Append variable and value lookups to codebook 
  cb$variables <- rbind(cb$variables, loc_variables)
  cb$values <- rbind(cb$values, loc_values)
  rm(loc_variables, loc_values)
  
  invisible(gc())
  cat('Appended person work location data.\n')
  
}

#============================================================================================#
# Person School

append_person_school_data <- function(data, location, cb) {
  data_file <- 'person'
  join_key <- get_table_keys(data_file)
  loc_cols <- c('STATEFIPS','CNTYFIPS','CBSA')
  new_loc_cols <- paste0('SCHL_', loc_cols)
  
  if(any(new_loc_cols %in% colnames(data$data$person))) {
    warning(paste(new_loc_cols, collapse = ', '),' already exist in the person dataset.')
    return()
  }
  
  # Merge location data
  data$data$person <- merge(
    x = data$data$person, 
    y = location[LOCTYPE == '03', c(join_key, loc_cols), with = F],
    all.x = T
  )
  
  # Create "Table-specific" location name
  setnames(data$data$person, loc_cols, new_loc_cols)
  
  # Replace NA values with "-1"
  data$data$person[, (new_loc_cols) := lapply(.SD, na2neg1), .SDcols = new_loc_cols]
  
  # Get variables and values
  loc_variables <- rbindlist(lapply(new_loc_cols, get_loc_variables, data_file = data_file))
  loc_values <- rbindlist(lapply(new_loc_cols, get_loc_values))
  
  # Append variable and value lookups to codebook 
  cb$variables <- rbind(cb$variables, loc_variables)
  cb$values <- rbind(cb$values, loc_values)
  rm(loc_variables, loc_values)
  
  invisible(gc())
  cat('Appended person school location data.\n')
  
}



#============================================================================================#
# Trip

append_trip_data <- function(data, location, cb) {
  data_file <- 'trip'
  join_key <- c(get_table_keys('household'), 'LOCNO')
  loc_cols <- c('STATEFIPS','CNTYFIPS','CBSA')
  new_loc_cols <- paste0('TRP_', loc_cols)
  
  if(any(new_loc_cols %in% colnames(data$data$trip))) {
    warning(paste(new_loc_cols, collapse = ', '),' already exist in the person dataset.')
    return()
  } 
  
  # Merge location data
  data$data$trip <- merge(
    x = data$data$trip, 
    y = location[, c(join_key, loc_cols), with = F],
    by = join_key,
    all.x = T
  )
  setkeyv(data$data$trip, get_table_keys(data_file))
  
  # Create "Table-specific" location name
  setnames(data$data$trip, loc_cols, new_loc_cols)
  
  # Replace NA values with "-1"
  data$data$trip[, (new_loc_cols) := lapply(.SD, na2neg1), .SDcols = new_loc_cols]
  
  # Get variables and values
  loc_variables <- rbindlist(lapply(new_loc_cols, get_loc_variables, data_file = data_file))
  loc_values <- rbindlist(lapply(new_loc_cols, get_loc_values))
  
  # Append variable and value lookups to codebook 
  cb$variables <- rbind(cb$variables, loc_variables)
  cb$values <- rbind(cb$values, loc_values)
  rm(loc_variables, loc_values)
  
  invisible(gc())
  cat('Appended trip location data.\n')
  
}

#============================================================================================#

#' @export
use_location_data <- function(data) {
  
  dataset <- data$dataset
  cb <- CB(dataset)
  
  if(dataset != '2017') {
    stop('Locations can only be appended to the 2017 dataset.')
  }
  
  location_file <- list.files(data$path, pattern = 'location.csv', full.names = T)
  if(length(location_file) == 0) {
    stop('location.csv is required and cannot be found in ', data$path)
  }
  
  select <- c('HOUSEID','LOCNO', 'PERSONID', 'LOCTYPE','STATEFIPS','CNTYFIPS','CBSA')
  location <- fread(location_file, select = select, colClasses = 'character')
  #setnames(location, colnames(location), toupper(colnames(location)))
  location[, CNTYFIPS := paste0(STATEFIPS, CNTYFIPS)]
  
  append_household_data(data, location, cb)
  append_person_work_data(data, location, cb)
  append_person_school_data(data, location, cb)
  append_trip_data(data, location, cb)
  
}

#============================================================================================#

join_shp_to_loc <- function(data, spdf) {
  
  location_file <- list.files(data$path, pattern = 'location.csv', full.names = T)
  if(length(location_file) == 0) {
    stop('location.csv is required and cannot be found in ', data$path)
  }
  
  select <- c('HOUSEID','LOCNO', 'PERSONID', 'LONGITUDE', 'LATITUDE')
  col_classes <- c(rep('character', 3), rep('numeric', 2))
  names(col_classes) <- select
  location <- fread(location_file, select = select, colClasses = col_classes)
  
  coordinates(location) = ~LONGITUDE+LATITUDE
  proj4string(location) <- proj4string(spdf)
  
  shp_loc <- over(location, spdf)
  shp_loc <- cbind(location@data, shp_loc)
  setDT(shp_loc)
  
  factors <- names(Filter(is.factor, shp_loc))
  shp_loc[, (factors) := lapply(.SD, as.character), .SDcols = factors]
  
  shp_loc[is.na(shp_loc)] <- '-1'
  
  return(shp_loc)
}


#============================================================================================#

#' @export
append_external_geography <- function(data, shp_file, shp_cols = NULL, loc_level = c('household','work','school','trip')) {
  cb <- CB('2017')
  
  message('Reading shape file...')
  layer <- gsub('.shp$', '', basename(shp_file))
  dsn <- dirname(shp_file)
  ext_shp <- readOGR(dsn, layer, verbose = FALSE)
  
  if (is.null(shp_cols)) {
    shp_cols <- names(ext_shp)
  }
  
  message('Joining shape file with location data...')
  shp_loc <- join_shp_to_loc(data, ext_shp)
  
  for(shp_col_prefix in shp_cols) {
    
    if ('household' %in% loc_level) {
      shp_col_class <- class(shp_loc[[shp_col_prefix]])
      var_name <- paste0(shp_col_prefix, '_HH')
      data$data$household[shp_loc[LOCNO == 100], (var_name) := get(shp_col_prefix), on = c('HOUSEID')]
      var_record <- data.table(NAME = var_name, TABLE = 'household', TYPE = shp_col_class, LABEL = var_name)
      cb$variables <- rbind(cb$variables[NAME != var_name], var_record)
      message('Added variable: ',var_name)
    }
    if ('work' %in% loc_level) {
      shp_col_class <- class(shp_loc[[shp_col_prefix]])
      var_name <- paste0(shp_col_prefix, '_WRK')
      data$data$person[shp_loc[grepl('^2[0-9]{2}$', LOCNO)], (var_name) := get(shp_col_prefix), on = c('HOUSEID','PERSONID')]
      var_record <- data.table(NAME = var_name, TABLE = 'person', TYPE = shp_col_class, LABEL = var_name)
      cb$variables <- rbind(cb$variables[NAME != var_name], var_record)
      message('Added variable: ',var_name)
    }
    if ('school' %in% loc_level) {
      shp_col_class <- class(shp_loc[[shp_col_prefix]])
      var_name <- paste0(shp_col_prefix, '_SCHL')
      data$data$person[shp_loc[grepl('^3[0-9]{2}$', LOCNO)], (var_name) := get(shp_col_prefix), on = c('HOUSEID','PERSONID')]
      var_record <- data.table(NAME = var_name, TABLE = 'person', TYPE = shp_col_class, LABEL = var_name)
      cb$variables <- rbind(cb$variables[NAME != var_name], var_record)
      message('Added variable: ',var_name)
    }
    if ('trip' %in% loc_level) {
      shp_col_class <- class(shp_loc[[shp_col_prefix]])
      var_name <- paste0(shp_col_prefix, '_TRP')
      data$data$trip[shp_loc, (var_name) := get(shp_col_prefix), on = c('HOUSEID','LOCNO')]
      var_record <- data.table(NAME = var_name, TABLE = 'trip', TYPE = shp_col_class, LABEL = var_name)
      cb$variables <- rbind(cb$variables[NAME != var_name], var_record)
      message('Added variable: ',var_name)
    } 
    
  }
}
