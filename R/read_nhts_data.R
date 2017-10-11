#' @title Read NHTS data.
#' 
#' @description Read and merge NHTS data for analysis.
#' 
#' @param dataset The study year of the dataset to read Currently only '2009'.
#' @param select A character vector of NHTS variable names to select for analysis. 
#' See \link[summarizeNHTS]{nhts_2009} for more info.
#' @param csv_path The parent directory of "/csv/dataset/". Defaults to working directory.
#' 
#' @details 
#' \code{read_nhts_data} is a wrapper for reading in variables from the correct csvs
#' and merging the corresponding tables on correct keys. \link[data.table]{fread} and
#' \link[data.table]{merge.data.table} are used for performance benefits. The data user, 
#' however, must still understand the relationships between the variables, weights, tables, 
#' etc. to ensure that analyses are being performed correctly. For example, if a trip level
#' variable is selected, this function assumes you will be performing a trip-level analysis 
#' and read in trip-level weights. It is recommended to make different \code{read_nhts_data}
#' calls for different "level" analyses (household, person, trip, vehicle).
#' 
#' @export
#' @import data.table
read_nhts_data <- function(dataset, select, csv_path = getwd()) {
  
  if(!dataset %in% c('2001','2009','2016')) {
    stop(dataset,' is not a valid dataset.')
  }
  
  path <- file.path(csv_path,'csv',dataset)
  
  if(length(list.files(path, '.csv', ignore.case = T)) == 0) {
    stop(
      "\nThe directory below does not exist or does not contain csv files.\n", path,
      "\n- Make sure the correct path is specified.",
      "\n- Or run ?download_nhts_data if you have not yet downloaded the data."
    )
  }
  
  #Check to see if NHTS Variables specified in select parameter exist.
  nhts_variables <- get(paste0('nhts_',dataset))[['variables']]
  
  select_match <- match(select, nhts_variables$DELIVERY_NAME)
  
  if(anyNA(select_match)) {
    invalid_variables <- paste(select[is.na(select_match)], collapse = ', ')
    stop(invalid_variables, ' are not valid variable names.')
  }
  
  #Subset NHTS Variables table by selected variables
  nhts_variables_selected <- nhts_variables[select_match]
  
  ###########################################################################################
  ################
  ## Trip Level ##
  ################
  if (nrow(nhts_variables_selected[DELIVERY_TABLE_NAME == 'trip']) > 0) {
    cat('\nReading Trip level variables.\n')
    
    trip_data <- fread(
      input = file.path(path, 'trip.csv'),
      select = c('HOUSEID','PERSONID','TDCASEID',nhts_variables_selected[DELIVERY_TABLE_NAME == 'trip', DELIVERY_NAME]),
      key = c('HOUSEID','PERSONID','TDCASEID')
    )
  }
  
  ##################
  ## Person Level ##
  ##################
  if (nrow(nhts_variables_selected[DELIVERY_TABLE_NAME == 'person']) > 0) {
    
    cat('\nReading Person level variables.\n')
    
    person_data <- fread(
      input = file.path(path, 'person.csv'),
      select = c('HOUSEID','PERSONID',nhts_variables_selected[DELIVERY_TABLE_NAME == 'person', DELIVERY_NAME]),
      key = c('HOUSEID','PERSONID')
    )
  }
  
  #####################
  ## Household Level ##
  #####################
  if (nrow(nhts_variables_selected[DELIVERY_TABLE_NAME == 'household']) > 0) {
    
    cat('\nReading Household level variables.\n')
    
    household_data <- fread(
      input = file.path(path, 'household.csv'),
      select = c('HOUSEID', nhts_variables_selected[DELIVERY_TABLE_NAME == 'household', DELIVERY_NAME]),
      key = c('HOUSEID')
    )
  }
  
  ###################
  ## VEHICLE Level ##
  ###################
  if (nrow(nhts_variables_selected[DELIVERY_TABLE_NAME == 'vehicle']) > 0) {
    
    cat('\nReading Vehicle level variables.\n')
    
    vehicle_data <- fread(
      input = file.path(path, 'vehicle.csv'),
      select = c('HOUSEID','VEHID', nhts_variables_selected[DELIVERY_TABLE_NAME == 'vehicle', DELIVERY_NAME]),
      key = c('HOUSEID','VEHID')
    )
  }
  
  ####################
  ## PERSON WEIGHTS ##
  ####################
  cat('\nReading Person level weights.\n')
  
  person_weights <- fread(
    input = file.path(path, 'person_weights.csv'),
    select = c('HOUSEID', 'PERSONID', get_wgt_names('WTPERFIN')),
    key = c('HOUSEID','PERSONID')
  )
  
  #######################
  ## HOUSEHOLD WEIGHTS ##
  #######################
  cat('\nReading Household level weights.\n')
  
  household_weights <- fread(
    input = file.path(path, 'household_weights.csv'),
    select = c('HOUSEID', get_wgt_names('HHWGT')),
    key = c('HOUSEID')
  )
  
  #########################
  ## TRIP EXPANSION KEYS ##
  #########################
  cat('\nReading trip Keys for trip weights.\n')
  
  if(exists('trip_data')) {
    trip_keys <- trip_data[, .(HOUSEID, PERSONID, TDCASEID)]
  } else {
    trip_keys <- fread(
      input = file.path(path, 'trip.csv'),
      select = c('HOUSEID','PERSONID','TDCASEID'),
      key = c('HOUSEID','PERSONID','TDCASEID')
    )
  }
  
  ###########################################################################################
  
  all_dt <- c('trip_data','person_data','household_data','vehicle_data')
  all_dt_exists <- sapply(all_dt, exists, where = environment())
  
  dt_list <- list(
    data = mget(all_dt[all_dt_exists]),
    weights = list(
      household = household_weights,
      person = person_weights,
      trip_keys = trip_keys
    )
  )
  
  #Strip "_data" from data.table list names
  names(dt_list$data) <- sapply(all_dt[all_dt_exists], function(x) sub('_data','',x))
  
  setattr(dt_list, 'dataset', dataset)
  
  invisible(gc())
  
  return(dt_list)
}
