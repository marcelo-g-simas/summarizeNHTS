#' Create weighted aggregate tables using NHTS data.
#'
#' @param data data.table object containing relavent NHTS variables and weights
#' @param agg Aggregate function label ("count","sum","avg", or "person_trip_rate")
#' @param agg_var Variable name to aggregate over. Only relavent when agg is "avg" or "sum"
#' @param factors Character element or vector of variable names to group by
#' @param variance Variance calculation to be used. Either "se" for Standard Error or "moe" for Margin of Error
#' @param subset A Pre-aggregation subset condition
#' @param jk_coeff Jacknife coefficient for standard error calculations
#' @return data.table object aggregated by input specifications
#' @export
#' @examples
#' # Example 1: Let's get the aveage number of vehicle miles by state and derived familiy income.
#' 
#' # Create vector of weight replicate names
#' wgt_names <- get_wgt_names('DAYWGT')
#' 
#' # Read in the two necessary datasets
#' per50wt <- fread('./data/2009/per50wt.csv', select = c('HOUSEID','PERSONID',wgt_names))
#' DAYV2PUB <- fread('./data/2009/DAYV2PUB.csv',select = c('HOUSEID','PERSONID','HHFAMINC','VMT_MILE','HHSTATE'))
#' 
#' # Set the appropriate unique identifiers for merging
#' setkeyv(per50wt, c('HOUSEID','PERSONID'))
#' setkeyv(DAYV2PUB, c('HOUSEID','PERSONID'))
#' 
#' # merge the datasets
#' dt <- DAYV2PUB[per50wt, nomatch=0]
#' 
#' ex3 <- make_table(
#'   data = dt, 
#'   agg = 'avg',
#'   agg_var = 'VMT_MILE',
#'   factors = c('HHSTATE','HHFAMINC'),
#'   wgt_name = 'DAYWGT',
#'   subset = 'VMT_MILE >= 0',
#'   variance = 'moe' #Option to get Margin of Error, instead of default Standard Error
#' )


make_table <- function(data, agg, agg_var = NULL, factors = NULL, subset = TRUE, label = FALSE, variance = 'se', jk_coeff = 99/100) {
  
  #creates vector of wgt names base on wgt_name prefix (i.e.: wgt_name1 - wgt_name100)
  wgt_base_names <- c("HHWGT", "WTPERFIN", "DAYWGT", "SFWGT")
  wgt_name <- wgt_base_names[wgt_base_names %in% colnames(data)]
  wgts <- get_wgt_names(wgt_name)
  
  #Get variables from data specified by the dataset attribute
  dataset <- attr(data, 'dataset')
  variables <- get(paste0('nhts_', dataset))[['variables']]
  
  if(agg == 'count') {
    
    #tryCatch(mget(factors), error = function(e) return(NULL))
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, Rcpp_sum), by = factors, .SDcols = wgts]
    
  } else if(agg == 'sum') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD*get(agg_var), Rcpp_sum), by = factors, .SDcols = wgts]
    
  } else if(agg == 'avg') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, Rcpp_wgtavg, x = get(agg_var)), by = factors, .SDcols = wgts]
    
  } else if(agg == 'person_trip_rate') {
    
    data <- data[eval(parse(text = subset)), ]
    
    #Get weighted trip count by all factors
    trip_count <- data[, lapply(.SD, Rcpp_sum), keyby = factors, .SDcols = wgts]
    
    #Grab the names of the variables that are not at the trip level
    non_trip_factors <- variables[Variable %in% factors & !Levels %in% c('Trip'), Variable]
    
    #Get distinct person keys, variables, and wgts
    distinct_person <- unique(data[, c('HOUSEID','PERSONID', non_trip_factors, wgts), with = FALSE])
    
    #Append persons with zero trips for "per-capita" trip rates (data.table attribte set in read_nhts_data)
    zero_trip_persons <- attr(data,'zero_trip_persons')[, colnames(distinct_person), with = FALSE]
    distinct_person <- rbind(distinct_person, zero_trip_persons)
    
    #Get weighted person counts by non trip factors
    person_count <- distinct_person[, lapply(.SD, Rcpp_sum), keyby = non_trip_factors, .SDcols = wgts]
    
    if(length(non_trip_factors) > 0) {
      #split the trip counts to a list of data.tables by non trip factors
      split_by_factor <- split(trip_count, by = non_trip_factors)
      
      #Get Trip rates by factor list
      trip_rate_list <- lapply(split_by_factor, function(x) {
        person <- merge(person_count, unique(x[, ..non_trip_factors]))[,..wgts]
        trip <- x[, ..wgts]
        sweep(as.matrix(trip), 2,  as.matrix(person), FUN = "/")
      })
      
      trip_rate <- do.call(rbind, trip_rate_list)
      
    } else {
      person <- person_count[, ..wgts]
      trip <- trip_count[, ..wgts]
      trip_rate <- sweep(as.matrix(trip), 2,  as.matrix(person), FUN = "/")
    }
    
    #Merge with factor combinations
    wgt_freq <- cbind(trip_count[, ..factors], trip_rate)
  
  } else stop(agg,' is not a valid aggregate label. Use "count", "sum", "avg", or "person_trip_rate".')
  
  fin_wgt <- as.matrix(wgt_freq[, wgts[1], with=F])
  rep_wgt <- as.matrix(wgt_freq[, wgts[-1], with=F])
  
  dif <- sweep(rep_wgt, 1, fin_wgt)^2
  se <- apply(dif, 1, function(x) sqrt(jk_coeff*sum(x)))
  
  tbl <- cbind(wgt_freq,se)[, !wgts[-1], with = F]
  tbl$moe <- tbl$se*1.984
  
  if(variance == 'se') {
    
    tbl <- tbl[, -'moe', with = F]
    
  } else if(variance == 'moe') {
    
    tbl <- tbl[, -'se', with = F]
    
  } else stop(sprintf('%s is not a valid aggregate label. Use "se" for Standard Error or "moe" for Margin of Error.', variance))
  
  #configure table output
  setnames(tbl, wgt_name, ifelse(!is.null(agg_var), agg_var, wgt_name))
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
 
  setattr(tbl, 'response', ifelse(!is.null(agg_var), agg_var, wgt_name))
  setattr(tbl, 'factors', factors)
  setattr(tbl, 'aggregate', switch(agg, count = 'Frequency', sum = 'Sum', avg = 'Average', person_trip_rate = 'Person Trip Rate'))
  setattr(tbl, 'variance', variance)
  setattr(tbl, 'dataset', dataset)
  
  # Assign labels to tabke if label parameter is TRUE
  if(label == T) tbl <- use_labels(tbl)
  
  return(tbl)
}

