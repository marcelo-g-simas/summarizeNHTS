#' Create weighted aggregate tables using NHTS data.
#'
#' @param data data.table object containing relavent NHTS variables and weights
#' @param agg Aggregate function label ("count","sum","avg", or "avg_trip_rate")
#' @param agg_var Variable name to aggregate over. Only relavent when agg is "avg" or "sum"
#' @param factors Character element or vector of variable names to group by
#' @param wgt_name Name of the weight variable being used
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


make_table <- function(data, agg, agg_var = NULL, factors = NULL, wgt_name, variance = 'se', subset = TRUE, jk_coeff = 99/100) {
  
  #creates vector of wgt names base on wgt_name prefix (i.e.: wgt_name1 - wgt_name100)
  wgts <- get_wgt_names(wgt_name)
  
  if(agg == 'count') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, sum), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else if(agg == 'sum') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD*get(agg_var), sum), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else if(agg == 'avg') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, function(x) sum(x*get(agg_var))/sum(x)), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else if(agg == 'avg_trip_rate') {
    
    group <- c('HOUSEID', 'PERSONID', factors)
    
    trp_counts <- dt[eval(parse(text = subset)), .(trps = .N), by = group]
    
    setkeyv(trp_counts,group)
    setkeyv(dt,group)
    
    dt <- trp_counts[unique(dt[, c(group, wgts), with = FALSE]), nomatch=0]
    
    wgt_freq <- dt[, lapply(.SD, function(x) sum(x*trps)/sum(x)), by = tryCatch(mget(factors), error = function(e) return(NULL)), .SDcols = wgts]
    
  } else stop(sprintf('%s is not a valid aggregate label. Use "count", "sum", "avg", or "avg_trip_rate".', agg))
  
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
  
  #save table attributes for future reference
  setattr(tbl, 'response', ifelse(!is.null(agg_var), agg_var, wgt_name))
  setattr(tbl, 'factors', factors)
  setattr(tbl, 'aggregate', switch(agg, count = 'Frequency', sum = 'Sum', avg = 'Average'))
  setattr(tbl, 'variance', variance)
  
  return(tbl)
}

