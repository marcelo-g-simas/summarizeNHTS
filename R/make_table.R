#' Create weighted aggregate tables using NHTS data.
#'
#' @param data Object returned by \link[summarizeNHTS]{read_nhts_data}
#' @param agg Aggregate function label. Either "household_count", "person_count", "trip_count", 
#' "sum", "avg", "household_trip_rate", or "person_trip_rate"
#' @param agg_var Character string specifying a numeric variable over which to aggregate. Only relavent when agg is "avg" or "sum"
#' @param factors Character vector of one or more variable names to group by
#' @param subset Character string containing a pre-aggregation subset condition using \link[data.table]{data.table} syntax
#' @param label logical. Use labels for table output?
#' @param prop logical. Use proportions for count aggregates?
#' @param prop_by Character vector of one or more variable names by which to group proportions
#' @return data.table object aggregated by input specifications
#' @export
make_table <- function(data, agg, agg_var = NULL, factors = NULL, subset = TRUE, label = TRUE, prop = FALSE, prop_by = NULL, exclude_missing = FALSE) {
  
  #Get variables from data specified by the dataset attribute
  dataset <- attr(data, 'dataset')
  variables <- get(paste0('nhts_', dataset))[['variables']]
  
  # Only select necessary variables
  data <- trim_input_data(
    data = data,
    variables = variables,
    factors = factors,
    agg_var = agg_var,
    subset = subset
  )
  
  # Exclude missing values in subset call
  if(exclude_missing == T) {
    subset <- exclude_missing_values(subset, vars = c(agg_var, factors))
  } else {
    subset <- exclude_missing_values(subset, vars = agg_var)
  }
  
  ##############################################################################################################
  ## COUNT AGGREGATES
  ##############################################################################################################
  if (agg %in% c('household_count','vehicle_count','person_count','trip_count')) {
    
    #==========================================================================================================#
    # CONFIGURE LEVEL 
    if (agg == 'household_count') {
      weight_table <- copy(data$weights$household)
      weight_names <- WGT('household')
      level_config <- 'household'
      pkey <- ID('household')
    } else if (agg == 'vehicle_count') {
      weight_table <- copy(data$weights$household)
      weight_names <- WGT('household')
      level_config <- c('household','vehicle')
      pkey <- c(ID('household'), ID('vehicle'))
    } else if (agg == 'person_count') {
      weight_table <- copy(data$weights$person)
      weight_names <- WGT('person')
      level_config <- c('household','person')
      pkey <- c(ID('household'), ID('person'))
    } else if (agg == 'trip_count') {
      weight_names <- WGT('trip')
      weight_table <- get_trip_weights(data)
      level_config <- c('household','person','trip')
      pkey <- c(ID('household'), ID('person'), ID('trip'))
    }
    
    #==========================================================================================================#
    # Drop factors if there is a level mismatch
    new_factors <- variables[DELIVERY_TABLE_NAME %in% level_config & DELIVERY_NAME %in% factors, DELIVERY_NAME]
    if (!all(factors %in% new_factors) & !is.null(factors)) {
      warning('agg: ', agg, '. Removing the following factors: ', paste(factors[!factors %in% new_factors], collapse = ', '))
      factors <- if (length(new_factors) == 0) NULL else new_factors
    }
    
    #==========================================================================================================#
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    rm(data)
    data_table <- unique(data_table[eval(parse(text = subset)), c(pkey, factors), with = F])
    setkeyv(data_table, pkey)
    data_table <- na.omit(data_table[weight_table, nomatch=0])
    
    #==========================================================================================================#
    # Compute weighted counts
    weighted_data <- data_table[, lapply(.SD, Rcpp_sum), by = factors, .SDcols = weight_names]
    
    #==========================================================================================================#
    # Compute unweighted counts
    unweighted_data <- data_table[, list(S = .N), by = factors]
    
    #==========================================================================================================#
    # Compute proportions
    if (prop == T) {
      weighted_data[, (weight_names) := lapply(.SD, prop.table), by = prop_by, .SDcols = weight_names]
      unweighted_data$S <- as.double(unweighted_data$S)
      unweighted_data[, S := prop.table(S), by = prop_by]
    }
    
    #==========================================================================================================#
    # Count data is same as unweighted data for count aggrergates
    count_data <- data_table[, .N, by = factors]
    
    ##############################################################################################################
    ## SUM/AVG AGGREGATES
    ##############################################################################################################
  } else if (agg %in% c('sum','avg')) {
    
    #==========================================================================================================#
    agg_level <- variables[DELIVERY_NAME == agg_var, DELIVERY_TABLE_NAME]
    pkey_level <- variables[DELIVERY_NAME %in% c(agg_var, factors), DELIVERY_TABLE_NAME]
    
    #==========================================================================================================#
    # CONFIGURE WEIGHTS 
    if (agg_level == 'household') {
      weight_table <- copy(data$weights$household)
      weight_names <- WGT('household')
      level_config <- c('household')
    } else if (agg_level == 'vehicle') {
      weight_table <- copy(data$weights$household)
      weight_names <- WGT('household')
      level_config <- c('household','vehicle')
    } else if (agg_level == 'person') {
      weight_table <- copy(data$weights$person)
      weight_names <- WGT('person')
      level_config <- c('household','person')
    } else if (agg_level == 'trip') {
      weight_names <- WGT('trip')
      weight_table <- get_trip_weights(data)
      level_config <- c('household','person','trip')
    }
    
    #==========================================================================================================#
    # CONFIGURE PRIMARY KEY LEVEL
    if (any(pkey_level == 'trip')) {
      pkey <- c(ID('household'), ID('person'), ID('trip'))
    } else if (any(pkey_level == 'person')) {
      pkey <- c(ID('household'), ID('person'))
    } else if (any(pkey_level == 'vehicle')) {
      pkey <- c(ID('household'), ID('vehicle'))
    } else if (any(pkey_level == 'household')) {
      pkey <- ID('household')
    }
    
    #==========================================================================================================#
    # Drop factors if there is a level mismatch
    new_factors <- variables[DELIVERY_TABLE_NAME %in% level_config & DELIVERY_NAME %in% factors, DELIVERY_NAME]
    if (!all(factors %in% new_factors) & !is.null(factors)) {
      warning('agg: ', agg, '. Removing the following factors: ', paste(factors[!factors %in% new_factors], collapse = ', '))
      factors <- if (length(new_factors) == 0) NULL else new_factors
    }
    
    #==========================================================================================================#
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    rm(data)
    data_table <- unique(data_table[eval(parse(text = subset)), c(pkey, factors, agg_var), with = F])
    setkeyv(data_table, pkey)
    data_table <- na.omit(data_table[weight_table, nomatch=0])
    
    #==========================================================================================================#
    # Compute count aggregate
    count_data <- data_table[, .N, by = factors]
    
    #==========================================================================================================#
    if (agg == 'sum') {
      
      weighted_data <- data_table[, lapply(.SD*get(agg_var), Rcpp_sum), by = factors, .SDcols = weight_names]
      unweighted_data <- data_table[, list(S = sum(get(agg_var))), by = factors]
      
    } else if (agg == 'avg') {
      
      weighted_data <- data_table[, lapply(.SD, Rcpp_wgtavg, x = get(agg_var)), by = factors, .SDcols = weight_names]
      unweighted_data <- data_table[, list(S = mean(get(agg_var))), by = factors]
      
    }
    
    ##############################################################################################################
    ## TRIP RATE AGGREGATES
    ##############################################################################################################
  } else if (agg %in% c('household_trip_rate','person_trip_rate')) {
    
    #==========================================================================================================#
    #Grab the names of the variables that are not at the trip level
    non_trip_factors <- variables[DELIVERY_NAME %in% factors & !DELIVERY_TABLE_NAME %in% c('trip'), DELIVERY_NAME]
    trip_factors <- factors[!factors %in% non_trip_factors]
    
    #==========================================================================================================#
    # CONFIGURE TRIP RATE LEVEL - Household or Person trip rates
    if (agg == 'household_trip_rate') {
      weight_table <- copy(data$weights$household)
      weight_names <- WGT('household')
      pkey <- ID('household')
    } else if (agg == 'person_trip_rate') {
      weight_table <- copy(data$weights$person)
      weight_names <- WGT('person')
      pkey <- c(ID('household'), ID('person'))
    }
    
    #==========================================================================================================#
    # Get trip weight names
    trip_weight_names <- WGT('trip')
    
    #==========================================================================================================#
    # Merge all data.tables
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    
    #==========================================================================================================#
    # Denominator - Household or Person Count
    pkey_distinct <- unique(data_table[eval(parse(text = subset)), c(pkey, non_trip_factors), with = F])
    pkey_weights <- merge(pkey_distinct, weight_table, by = pkey)
    pkey_count <- pkey_weights[, lapply(.SD, Rcpp_sum), keyby = non_trip_factors, .SDcols = weight_names]
    unweighted_pkey_count <- pkey_distinct[, .N, keyby = non_trip_factors]
    rm(pkey_distinct, pkey_weights)
    
    #==========================================================================================================#
    # Numerator - Trip Count
    trip_distinct <- na.omit(unique(data_table[eval(parse(text = subset)), c(ID('household'), ID('person'), ID('trip'), factors), with = F]))
    trip_weights <- merge(trip_distinct, get_trip_weights(data), by = c(ID('household'), ID('person'), ID('trip')))
    trip_count <- trip_weights[, lapply(.SD, Rcpp_sum), keyby = factors, .SDcols = trip_weight_names]
    unweighted_trip_count <- trip_distinct[, .N, keyby = factors]
    rm(data_table, trip_weights, data)
    
    #==========================================================================================================#
    # Sample Count
    count_data <- trip_distinct[, .N, by = factors]
    rm(trip_distinct)
    
    #==========================================================================================================#
    # weighted calculations
    if (length(non_trip_factors) > 0) {
      weighted_counts <- merge(trip_count, pkey_count)
    } else {
      weighted_counts <- cbind(trip_count, pkey_count)
    }
    
    #==========================================================================================================#
    # Element-wise division of trip_weights over pkey_weights
    weighted_trip_rates <- weighted_counts[, ..trip_weight_names] / weighted_counts[, ..weight_names]
    weighted_trip_rates <- weighted_trip_rates / 365 # Divide by 365 to get Daily rates
    colnames(weighted_trip_rates) <- weight_names
    
    #==========================================================================================================#
    # Append factors to weighted data if they exist
    if (!is.null(factors)) {
      weighted_data <- cbind(weighted_counts[,..factors], weighted_trip_rates)
    } else {
      weighted_data <- weighted_trip_rates
    }
    
    #==========================================================================================================#
    # unweighted calculations
    if (length(non_trip_factors) > 0) {
      unweighted_counts <- merge(unweighted_trip_count, unweighted_pkey_count, suffixes = c('_trip','_pkey'))
    } else {
      setnames(unweighted_pkey_count, 'N', 'N_pkey')
      setnames(unweighted_trip_count, 'N', 'N_trip')
      unweighted_counts <- cbind(unweighted_trip_count, unweighted_pkey_count)
    }
    unweighted_data <- unweighted_counts[, .(S = N_trip / N_pkey), keyby = factors]
    
    
  } else {
    stop(agg,' is not a valid aggregate label. Use "household_count", "vehicle_count", "person_count", "trip_count", "sum", "avg", "household_trip_rate", or "person_trip_rate".')
  }
  
  ################################################################################################################
  
  #==========================================================================================================#
  # Compute Standard Error (E)
  fin_wgt <- as.matrix(weighted_data[, weight_names[1], with=F])
  rep_wgt <- as.matrix(weighted_data[, weight_names[-1], with=F])
  dif <- sweep(rep_wgt, 1, fin_wgt)^2
  E <- apply(dif, 1, function(x) sqrt((99 / 100) * sum(x)))
  
  #==========================================================================================================#
  # Merge weighted (W), error (E), sampled/unweighted (S), and count (N) data
  weighted_data <- cbind(weighted_data[, !weight_names[-1], with = F], E)
  
  setkeyv(weighted_data, factors)
  setkeyv(unweighted_data, factors)
  setkeyv(count_data, factors)
  
  if (!is.null(factors)) {
    tbl <- Reduce(merge, list(weighted_data, unweighted_data, count_data))
  } else {
    tbl <- cbind(weighted_data, unweighted_data, count_data)
  }
  
  # Set column names and factor order
  setnames(tbl, weight_names[1], 'W')
  setorderv(tbl, factors)
  
  # Warn if prop = T with non-count aggregates
  if (prop == T & !agg %in% c('household_count','vehicle_count','person_count','trip_count')) {
    warning('Can only calculate proportions for count aggregates. Ignoring parameter "prop = TRUE".')
  }
  
  ##############################################################################################################
  # Set Table Attributes
  ##############################################################################################################
  
  #
  setattr(tbl, 'dataset',dataset)
  #
  setattr(tbl, 'agg_var', agg_var)
  setattr(tbl, 'agg_var_label', variables[DELIVERY_NAME == agg_var, DELIVERY_LABEL])
  setattr(tbl, 'factors', factors)
  setattr(tbl, 'error', 'Standard Error')
  setattr(tbl, 'prop', prop)
  setattr(tbl, 'agg_label', switch(agg, 
    household_count = 'Household Frequency',
    vehicle_count = 'Vehicle Count',
    person_count = 'Person Frequency',
    trip_count = 'Trip Frequency',
    sum = paste('Sum of', agg_var),
    avg = paste('Average', agg_var),
    person_trip_rate = 'Person Trip Rate',
    household_trip_rate = 'Household Trip Rate'
  ))
  if (!is.null(factors)) {
    setattr(tbl, 'factors_label', as.list(
      variables[DELIVERY_NAME %in% factors, mapply(function(x,y) cbind(x = y), x = DELIVERY_NAME, y = DELIVERY_LABEL)]
    ))
  } else {
    setattr(tbl, 'factors_label', list())
  }
  
  ##############################################################################################################
  
  # Assign labels to tabke if label parameter is TRUE
  if (label == T) tbl <- use_labels(tbl)
  
  # Make sure data.table key is set to the table factors if present
  if (!is.null(factors)) setkeyv(tbl, factors)
  
  # Garbage collection
  invisible(gc()) 
  
  return(tbl)
}

