#' @title Add Custom Variable.
#' 
#' @description Add custom variable to dataset and codebook.
#' 
#' @param data List of data.table obejcets returned by read_data(). 
#' @param custom_var Character. New custom variable name.
#' @param level Either "household", "person", "vehicle", or "trip".
#' @param label Description of custom variable. Defaults to custom_var value.
#' @param values Values of the custom variable. Must be same length as destination table. Not needed when using config_csv.
#' @param config_csv File path to a csv with fields "DOMAIN", "VALUE", "DESCRIPTION". Not needed when using values.
#' 
#' @export
#' @import data.table

custom_variable <- function(data, custom_var, level, data_type, label = custom_var, values = NULL, config_csv = NULL) {
  
  dataset <- attr(data, 'dataset')
  cb <- CB(dataset)
  
  if(!level %in% c('household','person','vehicle','trip')) {
    stop(level, ' is an invalid level value. Choose "household", "person", "vehicle", or "trip".') 
  }
  
  if(!is.null(config_csv)) {
    
    config_table <- read.csv(config_csv, stringsAsFactors = FALSE)
    
    if(!identical(colnames(config_table), c('DOMAIN','VALUE','DESCRIPTION'))) {
      stop('CSV must be a table with columns: "DOMAIN", "VALUE", "DESCRIPTION"')
    }
    
    # Add variable to dataset and bin by soecified domains
    for(i in 1:nrow(config_table)) {
      domain <- parse(text = config_table[i, 'DOMAIN'])
      value <- config_table[i, 'VALUE']
      data$data[[level]][eval(domain), (custom_var) := as.character(value)]
    }
    
    
    # new label for the codebook
    new_codebook_label <- data.table(
      NAME = custom_var,
      VALUE = as.character(config_table[, 'VALUE']),
      DESCRIPTION = config_table[, 'DESCRIPTION']
    )
    
  } else if(!is.null(values)) {
    
    dest_table_length <- nrow(data$data[[level]])
    
    if(dest_table_length != length(values)) {
      stop('Destination table row count (',dest_table_length,') is not equal to "values" length (',length(values),').')
    }
    
    data$data[[level]][, (custom_var) := values]
    new_codebook_label <- NULL
    
  } else (
    stop('Both "values" and "config_csv" cannot be NULL!')
  )
  
  # new variable to the codebook
  new_codebook_variable <- data.table(
    DELIVERY_NAME = custom_var,
    DELIVERY_TABLE_NAME = level,
    DELIVERY_LABEL = label,
    DATA_TYPE = data_type
  )
  
  if(nrow(cb$variables[DELIVERY_NAME == custom_var]) > 0 | nrow(cb$labels[NAME == custom_var]) > 0) {
    
    warning(custom_var, ' already exists. Overwriting existing data and codebook records.')
    
    cb$variables[DELIVERY_NAME == custom_var] <- new_codebook_variable
    cb$labels[NAME == custom_var] <- new_codebook_label
    
  } else {
    cb$variables <- rbind(cb$variables, new_codebook_variable)
    cb$labels <- rbind(cb$labels, new_codebook_label)
  }
  
  cat('\nSuccess!', custom_var, 'was added to the current session\'s dataset and codebook.')
}


