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


make_table <- function(data, agg, agg_var = NULL, factors = NULL, variance = 'se', subset = TRUE, jk_coeff = 99/100) {
  
  #creates vector of wgt names base on wgt_name prefix (i.e.: wgt_name1 - wgt_name100)
  wgt_base_names <- c("HHWGT", "WTPERFIN", "DAYWGT", "SFWGT")
  wgt_name <- wgt_base_names[wgt_base_names %in% colnames(data)]
  wgts <- get_wgt_names(wgt_name)
  
  #Get variables from data specified by the dataset attribute
  dataset <- attr(data, 'dataset')
  variables <- get(paste0('nhts_', dataset))[['variables']]
  
  if(agg == 'count') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, sum), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else if(agg == 'sum') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD*get(agg_var), sum), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else if(agg == 'avg') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, function(x) sum(x*get(agg_var))/sum(x)), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else if(agg == 'person_trip_rate') {
    
    #Add household-person identifier for counting person trips
    data <- data[, HPID := paste0(HOUSEID,PERSONID)][eval(parse(text = subset)),]
    
    #Trip factors are handled differently for trip rate calculations. Need to account for 0 trip factor combos.
    trip_factors <- variables[Variable %in% factors & Levels %in% c('Trip'), Variable]
    other_factors <- variables[Variable %in% factors & !Levels %in% c('Trip'), Variable]
    
    #Get existing trip counts
    trp_counts <- data[, .(trps = .N), by = c('HPID',factors)]
    setkeyv(trp_counts, c('HPID',factors))
    
    #Create factor combinations for each person.
    expanded <- data[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols= c('HPID',trip_factors)]
    expanded <- expanded[unique(data[, c('HPID',other_factors), with = F]), on = 'HPID']
    setkeyv(expanded, c('HPID',factors))
    
    #Merge existing trip counts with all factor cominations.
    merged <- trp_counts[expanded]
    merged[is.na(trps), trps := 0]
    
    if(nrow(merged) > 5000000) warning('Over 5,000,000 records result from expanded factor combinations. Calculations may be slow.')
    
    #Get distinct person weight records
    distinct_wgts <- unique(data[, c('HPID', wgts), with = FALSE])
    
    # Merging weights with every trip factor combination is resource-intensive. If more than one trip factor, then merge dynamically.
    if(length(trip_factors) < 2) {
      merged <- merged[distinct_wgts, on = 'HPID']
      wgt_freq <- merged[, lapply(.SD, function(x) sum(x*trps)/sum(x)), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    } else {
      wgt_freq <- merged[, lapply(distinct_wgts[HPID == HPID,-1], function(x) sum(x*trps)/sum(x)), by = tryCatch(mget(factors), error = function(e) return(NULL))]
    }
  
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
  
  return(tbl)
}

