#' Create weighted aggregate tables using NHTS data.
#'
#' @param data data.table object containing relavent NHTS variables and weights
#' @param agg Aggregate function label ("household_count", "person_count", "trip_count", 
#' "sum", "avg", "household_trip_rate", or "person_trip_rate")
#' @param agg_var Variable name to aggregate over. Only relavent when agg is "avg" or "sum"
#' @param factors Character element or vector of variable names to group by
#' @param subset A Pre-aggregation subset condition
#' @param label logical. Use labels for table output?
#' @param variance Variance calculation to be used. Either "se" for Standard Error or "moe" for Margin of Error
#' @param jk_coeff Jacknife coefficient for standard error calculations
#' @return data.table object aggregated by input specifications
#' @export
make_table <- function(data, agg, agg_var = NULL, factors = NULL, subset = TRUE, label = FALSE, prop = FALSE, prop_by = NULL) {
  
  #Get variables from data specified by the dataset attribute
  dataset <- attr(data, 'dataset')
  variables <- get(paste0('nhts_', dataset))[['variables']]
  
  ######################
  ## COUNT AGGREGATES ##
  ######################
  if (agg %in% c('household_count','vehicle_count','person_count','trip_count')) {
    
    ###################################################
    # CONFIGURE LEVEL 
    if (agg == 'household_count') {
      weight_table <- copy(data$weights$household)
      weight_names <- get_wgt_names("HHWGT")
      level_config <- 'household'
      pkey <- HHID
    } else if (agg == 'vehicle_count') {
      weight_table <- copy(data$weights$household)
      weight_names <- get_wgt_names("HHWGT")
      level_config <- c('household','vehicle')
      pkey <- c(HHID, VEHID)
    } else if (agg == 'person_count') {
      weight_table <- copy(data$weights$person)
      weight_names <- get_wgt_names("WTPERFIN")
      level_config <- c('household','person')
      pkey <- c(HHID, PERID)
    } else if (agg == 'trip_count') {
      weight_table <- copy(data$weights$person)
      temp_weight_names <- get_wgt_names("WTPERFIN")
      weight_names <- get_wgt_names('DAYWGT')
      weight_table[, (temp_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = temp_weight_names]
      weight_table <- weight_table[data$weights$trip_keys]
      setnames(weight_table, temp_weight_names, weight_names)
      level_config <- c('household','person','trip')
      pkey <- c(HHID, PERID, TRPID)
      setkeyv(weight_table, pkey)
    }
    ###################################################
    
    # Drop factors if there is a level mismatch
    new_factors <- variables[DELIVERY_TABLE_NAME %in% level_config & DELIVERY_NAME %in% factors, DELIVERY_NAME]
    if (!all(factors %in% new_factors) & !is.null(factors)) {
      warning('agg: ', agg, '. Removing the following factors: ', paste(factors[!factors %in% new_factors], collapse = ', '))
      factors <- if (length(new_factors) == 0) NULL else new_factors
    }
    
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    data_table <- unique(data_table[eval(parse(text = subset)), c(pkey, factors), with = F])
    setkeyv(data_table, pkey)
    data_table <- na.omit(data_table[weight_table, nomatch=0])
    
    # Compute weighted counts
    weighted_data <- data_table[, lapply(.SD, Rcpp_sum), by = factors, .SDcols = weight_names]
    
    # Compute unweighted counts
    unweighted_data <- data_table[, list(S = .N), by = factors]
    
    # Compute proportions
    if (prop == T) {
      weighted_data[, (weight_names) := lapply(.SD, prop.table), by = prop_by, .SDcols = weight_names]
      unweighted_data[, S := prop.table(S), by = prop_by]
    }
    
    # Count data is same as unweighted data for count aggrergates
    count_data <- data_table[, .N, by = factors]
    
    ########################
    ## SUM/AVG AGGREGATES ##
    ########################
  } else if (agg %in% c('sum','avg')) {
    
    agg_level <- variables[DELIVERY_NAME == agg_var, DELIVERY_TABLE_NAME]
    pkey_level <- variables[DELIVERY_NAME %in% c(agg_var, factors), DELIVERY_TABLE_NAME]
    
    # CONFIGURE WEIGHTS 
    if (agg_level == 'household') {
      weight_table <- copy(data$weights$household)
      weight_names <- get_wgt_names("HHWGT")
      level_config <- c('household')
    } else if (agg_level == 'vehicle') {
      weight_table <- copy(data$weights$household)
      weight_names <- get_wgt_names("HHWGT")
      level_config <- c('household','vehicle')
    } else if (agg_level == 'person') {
      weight_table <- copy(data$weights$person)
      weight_names <- get_wgt_names("WTPERFIN")
      level_config <- c('household','person')
    } else if (agg_level == 'trip') {
      weight_table <- copy(data$weights$person)
      temp_weight_names <- get_wgt_names("WTPERFIN")
      weight_names <- get_wgt_names('DAYWGT')
      weight_table[, (temp_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = temp_weight_names]
      weight_table <- data$weights$trip_keys[weight_table]
      setkeyv(weight_table, c(HHID, PERID, TRPID))
      setnames(weight_table, temp_weight_names, weight_names)
      level_config <- c('household','person','trip')
    }
    
    # CONFIGURE PRIMARY KEY LEVEL
    if (any(pkey_level == 'trip')) {
      pkey <- c(HHID, PERID, TRPID)
    } else if (any(pkey_level == 'person')) {
      pkey <- c(HHID, PERID)
    } else if (any(pkey_level == 'vehicle')) {
      pkey <- c(HHID, VEHID)
    } else if (any(pkey_level == 'household')) {
      pkey <- HHID
    }
    
    # Drop factors if there is a level mismatch
    new_factors <- variables[DELIVERY_TABLE_NAME %in% level_config & DELIVERY_NAME %in% factors, DELIVERY_NAME]
    if (!all(factors %in% new_factors) & !is.null(factors)) {
      warning('agg: ', agg, '. Removing the following factors: ', paste(factors[!factors %in% new_factors], collapse = ', '))
      factors <- if (length(new_factors) == 0) NULL else new_factors
    }
    
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    data_table <- unique(data_table[eval(parse(text = subset)), c(pkey, factors, agg_var), with = F])
    setkeyv(data_table, pkey)
    data_table <- na.omit(data_table[weight_table, nomatch=0])
    
    # Compute count aggregate
    count_data <- data_table[, .N, by = factors]
    
    if (agg == 'sum') {
      
      weighted_data <- data_table[, lapply(.SD*get(agg_var), Rcpp_sum), by = factors, .SDcols = weight_names]
      unweighted_data <- data_table[, list(S = sum(get(agg_var))), by = factors]
      
    } else if (agg == 'avg') {
      
      weighted_data <- data_table[, lapply(.SD, Rcpp_wgtavg, x = get(agg_var)), by = factors, .SDcols = weight_names]
      unweighted_data <- data_table[, list(S = mean(get(agg_var))), by = factors]
      
    }
    
    ##########################
    ## TRIP RATE AGGREGATES ##
    ##########################  
  } else if (agg %in% c('household_trip_rate','person_trip_rate')) {
    
    #Grab the names of the variables that are not at the trip level
    non_trip_factors <- variables[DELIVERY_NAME %in% factors & !DELIVERY_TABLE_NAME %in% c('trip'), DELIVERY_NAME]
    trip_factors <- factors[!factors %in% non_trip_factors]
    
    # CONFIGURE TRIP RATE LEVEL
    if (agg == 'household_trip_rate') {
      weight_table <- copy(data$weights$household)
      weight_table <- weight_table
      temp_weight_names <- get_wgt_names("HHWGT")
      weight_names <- get_wgt_names('DAYWGT')
      pkey <- HHID
    } else if (agg == 'person_trip_rate') {
      weight_table <- copy(data$weights$person)
      temp_weight_names <- get_wgt_names("WTPERFIN")
      weight_names <- get_wgt_names('DAYWGT')
      pkey <- c(HHID, PERID)
    }
    
    # SETUP WEIGHT TABLE
    weight_table[, (temp_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = temp_weight_names]
    setnames(weight_table, temp_weight_names, weight_names)
    
    # Merge all data.tables
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    
    # Get distinct person and trip records
    distinct_pkey <- unique(data_table[eval(parse(text = subset)), c(pkey, factors), with = F])
    distinct_trips <- na.omit(unique(data_table[eval(parse(text = subset)), c(HHID, PERID, TRPID, factors), with = F]))
    
    # Compute count aggregate
    count_data <- distinct_trips[, .N, by = factors]
    rm(data_table)
    
    # pkey count calculation
    setkeyv(distinct_pkey, pkey)
    distinct_pkey <- na.omit(distinct_pkey[weight_table])
    pkey_count <- distinct_pkey[, lapply(.SD, Rcpp_sum), keyby = non_trip_factors, .SDcols = weight_names]
    
    # trip count calculation
    setkeyv(distinct_trips, c(HHID, PERID, TRPID))
    weight_table <-  weight_table[data$weights$trip_keys]
    setkeyv(weight_table, c(HHID, PERID, TRPID))
    trip_count <- weight_table[distinct_trips][, lapply(.SD, Rcpp_sum), keyby = factors, .SDcols = weight_names]
    
    # weighted calculations
    if (length(non_trip_factors) > 0) {
      weighted_counts <- merge(trip_count, pkey_count, suffixes = c('_trip','_pkey'))
    } else {
      setnames(trip_count, weight_names, paste0(weight_names,'_trip'))
      setnames(pkey_count, weight_names, paste0(weight_names,'_pkey'))
      weighted_counts <- cbind(trip_count, pkey_count)
    }
    trip_cols <- colnames(weighted_counts)[grepl('_trip$',colnames(weighted_counts))]
    pkey_cols <- colnames(weighted_counts)[grepl('_pkey$',colnames(weighted_counts))]
    weighted_trip_rates <- weighted_counts[, ..trip_cols] / weighted_counts[, ..pkey_cols]
    colnames(weighted_trip_rates) <- weight_names
    if (!is.null(factors)) {
      weighted_data <- cbind(weighted_counts[,..factors], weighted_trip_rates)
    } else {
      weighted_data <- weighted_trip_rates
    }
    
    
    # unweighted calculations
    unweighted_pkey_count <- distinct_pkey[, .N, keyby = non_trip_factors]
    unweighted_trip_count <- distinct_trips[, .N, keyby = factors]
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
  
  # Compute Standard Error (E)
  fin_wgt <- as.matrix(weighted_data[, weight_names[1], with=F])
  rep_wgt <- as.matrix(weighted_data[, weight_names[-1], with=F])
  dif <- sweep(rep_wgt, 1, fin_wgt)^2
  E <- apply(dif, 1, function(x) sqrt((99 / 100) * sum(x)))
  
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
  
  ################################################################################################################
  # Set Table Attributes
  ################################################################################################################
  
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

  ################################################################################################################

  # Assign labels to tabke if label parameter is TRUE
  if (label == T) tbl <- use_labels(tbl)
  
  # Coerce factor variables as "factors" and retain value order
  if (!is.null(factors)) {
    #tbl[, factors] <- lapply(tbl[, factors, with = F], function(x) factor(x, levels = x))
    setkeyv(tbl, factors)
  }

  invisible(gc())
  
  return(tbl)
}

