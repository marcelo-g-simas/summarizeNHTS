#' @export
read_nhts_data <- function(dataset, select, csv_path = getwd()) {
  
  if(!dataset %in% c('2009')) {
    stop(dataset,' is not a valid dataset.')
  }
  
  path <- file.path(csv_path,'csv',dataset)
  
  nhts_variables <- get(paste0('nhts_',dataset))[['variables']]
  
  select_match <- match(select, nhts_variables$Variable)
  
  if(anyNA(select_match)) {
    invalid_variables <- paste(select[is.na(select_match)], collapse = ', ')
    stop(invalid_variables, ' are not valid variable names.')
  }
  
  nhts_variables_selected <- nhts_variables[select_match]
  
  
  if (nrow(nhts_variables_selected[Levels == 'Trip']) > 0) {
    
    cat('\nReading Trip level variables.\n')
    
    trip_data <- fread(
      input = file.path(path, 'DAYV2PUB.csv'),
      select = c('HOUSEID','PERSONID','TDCASEID',nhts_variables_selected[Levels == 'Trip', Variable]),
      key = c('HOUSEID','PERSONID','TDCASEID')
    )
    
    cat('\nReading Trip level replicate weights.\n')
    
    trip_weights <- fread(
      input = file.path(path, 'per50wt.csv'), 
      select = c('HOUSEID','PERSONID', get_wgt_names('DAYWGT')),
      key = c('HOUSEID','PERSONID')
    )
    
  }
  
  if (nrow(nhts_variables_selected[Levels == 'Person']) > 0) {
    
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
  
  if (nrow(nhts_variables_selected[Levels == 'Household']) > 0) {
    
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
  
  all_dt <- c('trip_data','trip_weights','person_data','person_weights','household_data','household_weights')
  
  dt <- Reduce(merge, mget(all_dt[sapply(all_dt, exists, where = environment())]))
  
  rm(all_dt)
  invisible(gc())
  
  return(dt)
}
