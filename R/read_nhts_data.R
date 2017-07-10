#' @title Read NHTS data.
#' 
#' @description Read and merge NHTS data for analysis.
#' 
#' @param dataset The study year of the dataset to read Currently only '2009'.
#' @param select A character vector of NHTS variable names to select for analysis. 
#' See \link[NHTSsummarizer]{nhts_2009} for more info.
#' @param csv_path The parent directory of "/csv/dataset/". Defaults to working directory.
#' @param level Either "Trip", "Person", or "Household". Helpful for expanding levels if
#' only lower level variables are specified in select parameter.
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
read_nhts_data <- function(dataset, select, csv_path = getwd(), level = '') {
  
  if(!dataset %in% c('2009')) {
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
  
  select_match <- match(select, nhts_variables$Variable)
  
  if(anyNA(select_match)) {
    invalid_variables <- paste(select[is.na(select_match)], collapse = ', ')
    stop(invalid_variables, ' are not valid variable names.')
  }
  
  #Subset NHTS Variables table by selected variables
  nhts_variables_selected <- nhts_variables[select_match]
  
  #If level parameter is specified, throw warning if invalid.
  if(!level %in% c('','Trip','Person','Household')) warning(level,' is not a valid level. Ignoring parameter.')
  
  ################
  ## Trip Level ##
  ################
  if (nrow(nhts_variables_selected[Levels == 'Trip']) > 0 | level == 'Trip') {
    
    cat('\nReading Trip level variables.\n')
    
    trip_data <- fread(
      input = file.path(path, 'DAYV2PUB.csv'),
      select = c('HOUSEID','PERSONID','TDCASEID',nhts_variables_selected[Levels == 'Trip', Variable]),
      key = c('HOUSEID','PERSONID','TDCASEID')
    )
    
    cat('\nReading Trip level replicate weights.\n')
    
    trip_weights <- fread(
      input = file.path(path, 'per50wt.csv'), 
      select = c('HOUSEID','PERSONID', get_wgt_names('WTPERFIN')),
      key = c('HOUSEID','PERSONID')
    )
    
    #Persons with zero trips do not have trip weights (only person level weights). Therefore:
    #Need to use person weights and multiply by 365 to get trip weigts
    person_wgt_names <- get_wgt_names('WTPERFIN')
    trip_weights[, (person_wgt_names) := lapply(.SD, function(x) x * 365), .SDcols = person_wgt_names]
    setnames(trip_weights, person_wgt_names, get_wgt_names('DAYWGT'))
    
  }
  
  ##################
  ## Person Level ##
  ##################
  if (nrow(nhts_variables_selected[Levels == 'Person']) > 0 | level == 'Person') {
    
    cat('\nReading Person level variables.\n')
    
    person_data <- fread(
      input = file.path(path, 'PERV2PUB.CSV'),
      select = c('HOUSEID','PERSONID',nhts_variables_selected[Levels == 'Person', Variable]),
      key = c('HOUSEID','PERSONID')
    )
    
    if(!exists('trip_weights')) {
      
      cat('\nReading Person level replicate weights.\n')
      
      person_weights <- fread(
        input = file.path(path, 'per50wt.csv'), 
        select = c('HOUSEID','PERSONID', get_wgt_names('WTPERFIN')),
        key = c('HOUSEID','PERSONID')
      )
      
    }
    
  }
  
  #####################
  ## Household Level ##
  #####################
  if (nrow(nhts_variables_selected[Levels == 'Household']) > 0 | level == 'Household') {
    
    cat('\nReading Household level variables.\n')
    
    household_data <- fread(
      input = file.path(path, 'HHV2PUB.CSV'),
      select = c('HOUSEID', nhts_variables_selected[Levels == 'Household', Variable]),
      key = c('HOUSEID')
    )
    
    if(!exists('trip_weights') & !exists('person_weights')) {
      
      cat('\nReading Household level replicate weights.\n')
      
      household_weights <- fread(
        input = file.path(path, 'hh50wt.csv'), 
        select = c('HOUSEID', get_wgt_names('HHWGT')),
        key = c('HOUSEID')
      )
    }
    
  }
  
  ######################################################################
  no_trips <- trip_weights[!trip_data]
  hp_dt <- c('no_trips','person_data','household_data')
  hp_dt_exists <- hp_dt[sapply(hp_dt, exists, where = environment())]
  
  if(length(hp_dt_exists) > 1) {
    zero_trip_persons <- Reduce(merge, mget(hp_dt_exists))
  } else {
    zero_trip_persons <- no_trips
  }
  ######################################################################
  
  all_dt <- c('trip_data','trip_weights','person_data','person_weights','household_data','household_weights')
  
  dt <- Reduce(merge, mget(all_dt[sapply(all_dt, exists, where = environment())]))
  setattr(dt, 'dataset', dataset)
  setattr(dt, 'zero_trip_persons', zero_trip_persons)
  
  rm(all_dt)
  invisible(gc())
  
  return(dt)
}
