#' @title Add derived Variables from a configuration worksheet (csv).
#' 
#' @description Add custom variable to dataset and codebook.
#' 
#' @param data Object returned by \link[summarizeNHTS]{read_data}. 
#' @param config_csv File path to a csv with fields "NAME", "TABLE", "TYPE", "DOMAIN", "VALUE", "LABEL".
#' 
#' @export
#' @import data.table

derived_variables <- function(data, config_csv = NULL) {
  
  if (!'HTS.data' %in% class(data)) {
    stop('data is not an "HTS.data" object (returned by the read_data function).')
  }
  
  dataset <- data$dataset
  cb <- CB(dataset)
  
  config_table <- fread(config_csv)
  
  if(!identical(colnames(config_table), c('NAME','TABLE','TYPE','DOMAIN','VALUE','LABEL'))) {
    stop('CSV must be a table with header: "NAME", "TABLE", "TYPE", "DOMAIN", "VALUE", "LABEL"')
  }
  
  config_table_list <- split(config_table, by = 'NAME')
  
  out <- lapply(config_table_list, function(tbl) {
    if (!check_config(tbl)) return(FALSE)
    append_config_data(tbl, data)
    add_config_codebook(tbl, cb)
    return(TRUE)
  })
  
  successful_variables <- names(which(unlist(out)))
  if (length(successful_variables) > 0) {
    message('Derived variables:\n', paste(successful_variables, collapse = ', '), ' successfully added!\n')
  }
  
  
}

check_config <- function(tbl) {
  
  NAME <- unique(tbl$NAME)
  TABLE <- unique(tbl$TABLE)
  TYPE <- unique(tbl$TYPE)
  PASS <- TRUE
  
  # TABLE Checks
  if (length(TABLE) != 1) {
    warning('More than 1 unique TABLE values specified for custom variable: ', NAME)
    PASS <- FALSE
  } else if (!TABLE %in% c('household','person','vehicle','trip')) {
    warning('In custom_variable: ', NAME, ', "', TABLE, '" is an invalid TABLE value. ',
            'Use "household", "person", "vehicle", or "trip."')
    PASS <- FALSE
  }
  
  # TYPE Checks
  if (length(TYPE) != 1) {
    warning('More than 1 unique TYPE values specified for custom variable: ', NAME)
    PASS <- FALSE
  } else if (!TYPE %in% c('character','numeric')) {
    warning('In custom_variable: ', NAME, ', "', TYPE, '" is an invalid TYPE value. ',
            'Use "character" or "numeric".') 
    PASS <- FALSE
  }
  
  return(PASS)
}

append_config_data <- function(tbl, data) {
  
  NAME <- unique(tbl$NAME)
  TABLE <- unique(tbl$TABLE)
  TYPE <- unique(tbl$TYPE)
  
  # Add variable to dataset and bin by soecified domains
  for(i in 1:nrow(tbl)) {
    DOMAIN <- parse(text = tbl[i, 'DOMAIN'])
    VALUE <- tbl[i, 'VALUE']
    data$data[[TABLE]][eval(DOMAIN), (NAME) := as.character(VALUE)]
  }
  
}

add_config_codebook <- function(tbl, cb) {
  
  DERIVED_NAME <- unique(tbl$NAME)
  DERIVED_TABLE <- unique(tbl$TABLE)
  DERIVED_TYPE <- unique(tbl$TYPE)
  
  # new label for the codebook
  new_codebook_values <- data.table(
    NAME = DERIVED_NAME,
    VALUE = as.character(tbl$VALUE),
    LABEL = tbl$LABEL
  )
  
  new_codebook_variable <- data.table(
    NAME = DERIVED_NAME,
    TABLE = DERIVED_TABLE,
    TYPE = DERIVED_TYPE,
    LABEL = DERIVED_NAME
  )
  
  if(DERIVED_NAME %in% cb$variables$NAME | DERIVED_NAME %in% cb$values$NAME) {
    warning(DERIVED_NAME, ' already exists. Overwriting existing data and codebook records.')
    cb$variables[NAME == DERIVED_NAME] <- new_codebook_variable
    cb$values[NAME == DERIVED_NAME] <- new_codebook_values
  } else {
    cb$variables <- rbind(cb$variables, new_codebook_variable)
    cb$values <- rbind(cb$values, new_codebook_values)
  }
  
} 