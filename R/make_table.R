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
make_table <- function(data, agg, agg_var = NULL, factors = NULL, subset = TRUE, label = FALSE, variance = 'se', jk_coeff = 99/100) {
  
  #Get variables from data specified by the dataset attribute
  dataset <- attr(data, 'dataset')
  variables <- get(paste0('nhts_', dataset))[['variables']]
  
  ######################
  ## COUNT AGGREGATES ##
  ######################
  if(agg %in% c('household_count','vehicle_count','person_count','trip_count')) {
    
    ############################################################################
    # CONFIGURE LEVEL 
    if(agg == 'household_count') {
      weight_table <- data$weights$household
      weight_names <- get_wgt_names("HHWGT")
      level_config <- 'Household'
      pkey <- 'HOUSEID'
    } else if(agg == 'vehicle_count') {
      weight_table <- data$weights$household
      weight_names <- get_wgt_names("HHWGT")
      level_config <- c('Household','Vehicle')
      pkey <- c('HOUSEID','VEHID')
    } else if (agg == 'person_count') {
      weight_table <- data$weights$person
      weight_names <- get_wgt_names("WTPERFIN")
      level_config <- c('Household','Person')
      pkey <- c('HOUSEID','PERSONID')
    } else if (agg == 'trip_count') {
      weight_table <- data$weights$person
      temp_weight_names <- get_wgt_names("WTPERFIN")
      weight_names <- get_wgt_names('DAYWGT')
      weight_table[, (temp_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = temp_weight_names]
      weight_table <- weight_table[data$weights$trip_keys]
      setnames(weight_table, temp_weight_names, weight_names)
      level_config <- c('Household','Person','Trip')
      pkey <- c('HOUSEID','PERSONID','TDCASEID')
      setkeyv(weight_table, pkey)
    }
    ############################################################################
    
    # Drop factors if there is a level mismatch
    new_factors <- variables[Levels %in% level_config & Variable %in% factors, Variable]
    if(!all(factors %in% new_factors) & !is.null(factors)) {
      warning('agg: ', agg, '. Removing the following factors: ', paste(factors[!factors %in% new_factors], collapse = ', '))
      factors <- if(length(new_factors) == 0) NULL else new_factors
    }
    
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    data_table <- unique(data_table[eval(parse(text = subset)), c(pkey, factors), with = F])
    setkeyv(data_table, pkey)
    data_table <- na.omit(data_table[weight_table])
    
    # Compute weighted counts
    weighted_data <- data_table[, lapply(.SD, Rcpp_sum), by = factors, .SDcols = weight_names]
    
    ########################
    ## SUM/AVG AGGREGATES ##
    ########################
  } else if(agg %in% c('sum','avg')) {
    
    agg_level <- variables[Variable == agg_var, Levels]
    pkey_level <- variables[Variable %in% c(agg_var, factors), Levels]
    
    # CONFIGURE WEIGHTS 
    if(agg_level == 'Household') {
      weight_table <- data$weights$household
      weight_names <- get_wgt_names("HHWGT")
      level_config <- c('Household')
    } else if (agg_level == 'Vehicle') {
      weight_table <- data$weights$household
      weight_names <- get_wgt_names("HHWGT")
      level_config <- c('Household','Vehicle')
    } else if (agg_level == 'Person') {
      weight_table <- data$weights$person
      weight_names <- get_wgt_names("WTPERFIN")
      level_config <- c('Household','Person')
    } else if (agg_level == 'Trip') {
      weight_table <- data$weights$person
      temp_weight_names <- get_wgt_names("WTPERFIN")
      weight_names <- get_wgt_names('DAYWGT')
      weight_table[, (temp_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = temp_weight_names]
      weight_table <- data$weights$trip_keys[weight_table]
      setkey(weight_table, HOUSEID, PERSONID, TDCASEID)
      setnames(weight_table, temp_weight_names, weight_names)
      level_config <- c('Household','Person','Trip')
    }
    
    # CONFIGURE PRIMARY KEY LEVEL
    if(any(pkey_level == 'Trip')) {
      pkey <- c('HOUSEID','PERSONID','TDCASEID')
    } else if(any(pkey_level == 'Person')) {
      pkey <- c('HOUSEID','PERSONID')
    } else if(any(pkey_level == 'Vehicle')) {
      pkey <- c('HOUSEID','VEHID')
    } else if(any(pkey_level == 'Household')) {
      pkey <- 'HOUSEID'
    }
    
    # Drop factors if there is a level mismatch
    new_factors <- variables[Levels %in% level_config & Variable %in% factors, Variable]
    if(!all(factors %in% new_factors) & !is.null(factors)) {
      warning('agg: ', agg, '. Removing the following factors: ', paste(factors[!factors %in% new_factors], collapse = ', '))
      factors <- if(length(new_factors) == 0) NULL else new_factors
    }
    
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    data_table <- unique(data_table[eval(parse(text = subset)), c(pkey, factors, agg_var), with = F])
    setkeyv(data_table, pkey)
    data_table <- na.omit(data_table[weight_table])
    
    if(agg == 'sum') {
      
      weighted_data <- data_table[, lapply(.SD*get(agg_var), Rcpp_sum), by = factors, .SDcols = weight_names]
      
    } else if (agg == 'avg') {
      
      weighted_data <- data_table[, lapply(.SD, Rcpp_wgtavg, x = get(agg_var)), by = factors, .SDcols = weight_names]
      
    }
    
    ##########################
    ## TRIP RATE AGGREGATES ##
    ##########################  
  } else if(agg %in% c('household_trip_rate','person_trip_rate')) {
    
    #Grab the names of the variables that are not at the trip level
    non_trip_factors <- variables[Variable %in% factors & !Levels %in% c('Trip'), Variable]
    trip_factors <- factors[!factors %in% non_trip_factors]
    
    # CONFIGURE TRIP RATE LEVEL
    if(agg == 'household_trip_rate') {
      weight_table <- copy(data$weights$household)
      weight_table <- weight_table
      temp_weight_names <- get_wgt_names("HHWGT")
      weight_names <- get_wgt_names('DAYWGT')
      pkey <- c('HOUSEID')
    } else if(agg == 'person_trip_rate') {
      weight_table <- copy(data$weights$person)
      temp_weight_names <- get_wgt_names("WTPERFIN")
      weight_names <- get_wgt_names('DAYWGT')
      pkey <- c('HOUSEID','PERSONID')
    }
    
    # SETUP WEIGHT TABLE
    weight_table[, (temp_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = temp_weight_names]
    setnames(weight_table, temp_weight_names, weight_names)
    
    # Merge all data.tables
    data_table <- Reduce(function(...) merge(..., allow.cartesian = T, all = T), data$data)
    
    distinct_pkey <- unique(data_table[eval(parse(text = subset)), c(pkey, factors), with = F])
    distinct_trips <- na.omit(unique(data_table[eval(parse(text = subset)), c('HOUSEID','PERSONID','TDCASEID', factors), with = F]))
    rm(data_table)
    
    # pkey count calculation
    setkeyv(distinct_pkey, pkey)
    distinct_pkey <- na.omit(distinct_pkey[weight_table])
    pkey_count <- distinct_pkey[, lapply(.SD, Rcpp_sum), keyby = non_trip_factors, .SDcols = weight_names]
    
    # trip count calculation
    setkey(distinct_trips, HOUSEID, PERSONID, TDCASEID)
    weight_table <-  weight_table[data$weights$trip_keys]
    setkey(weight_table, HOUSEID, PERSONID, TDCASEID)
    trip_count <- weight_table[distinct_trips][, lapply(.SD, Rcpp_sum), keyby = factors, .SDcols = weight_names]
    
    if(length(non_trip_factors) > 0) {
      #split the trip counts to a list of data.tables by non trip factors
      split_by_factor <- split(trip_count, by = non_trip_factors)
      
      #Get Trip rates by factor list
      trip_rate_list <- lapply(split_by_factor, function(x) {
        hp <- merge(pkey_count, unique(x[, ..non_trip_factors]))[,..weight_names]
        trip <- x[, ..weight_names]
        sweep(as.matrix(trip), 2,  as.matrix(hp), FUN = "/")
      })
      
      trip_rate <- do.call(rbind, trip_rate_list)
      
    } else {
      pkey <- pkey_count[, ..weight_names]
      trip <- trip_count[, ..weight_names]
      trip_rate <- sweep(as.matrix(trip), 2,  as.matrix(hp), FUN = "/")
    }
    
    # Because data.table is first split by non_trip_factors, must order them first
    factor_order <- c(non_trip_factors, trip_factors)
    setkeyv(trip_count, factor_order)
    
    #Merge with factor combinations
    weighted_data <- cbind(trip_count[, ..factors], trip_rate)
    
  } else {
    stop(agg,' is not a valid aggregate label. Use "household_count", "person_count", "trip_count", "sum", "avg", "household_trip_rate", or "person_trip_rate".')
  }
  
  fin_wgt <- as.matrix(weighted_data[, weight_names[1], with=F])
  rep_wgt <- as.matrix(weighted_data[, weight_names[-1], with=F])
  
  dif <- sweep(rep_wgt, 1, fin_wgt)^2
  se <- apply(dif, 1, function(x) sqrt(jk_coeff*sum(x)))
  
  tbl <- cbind(weighted_data, se)[, !weight_names[-1], with = F]
  tbl$moe <- tbl$se*1.984
  
  if(variance == 'se') {
    
    tbl <- tbl[, -'moe', with = F]
    
  } else if(variance == 'moe') {
    
    tbl <- tbl[, -'se', with = F]
    
  } else stop(sprintf('%s is not a valid aggregate label. Use "se" for Standard Error or "moe" for Margin of Error.', variance))
  
  #configure table output
  setnames(tbl, weight_names[1], ifelse(!is.null(agg_var), agg_var, agg))
  setorderv(tbl, factors)
  
  #Set Table Attributes#
  ######################
  
  #response variable
  if(!is.null(agg_var)) {
    setattr(tbl, 'response_label', variables[Variable == agg_var, Description])
  } else if(agg == 'count') {
    setattr(tbl, 'response_label', 'Frequency')
  } else if(agg == 'person_trip_rate') {
    setattr(tbl, 'response_label', 'Average Person Trips Per Day')
  } else setattr(tbl, 'response_label', '')
  
  #factor variables
  setattr(tbl, 'factors_label', as.list(variables[Variable %in% factors, mapply(function(x,y) cbind(x = y), x = Variable, y = Description)]))
  
  setattr(tbl, 'response', ifelse(!is.null(agg_var), agg_var, agg))
  setattr(tbl, 'factors', factors)
  setattr(tbl, 'aggregate', switch(agg, count = 'Frequency', sum = 'Sum', avg = 'Average', person_trip_rate = 'Person Trip Rate'))
  setattr(tbl, 'variance', variance)
  setattr(tbl, 'dataset', dataset)
  
  # Assign labels to tabke if label parameter is TRUE
  if(label == T) tbl <- use_labels(tbl)
  
  invisible(gc())
  
  return(tbl)
}

