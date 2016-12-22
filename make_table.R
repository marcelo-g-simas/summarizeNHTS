

make_table <- function(data, agg, agg_var = NULL, factors, wgt_name, variance = 'se', subset = TRUE, jk_coeff = 99/100) {
  
  #creates vector of wgt names base on wgt_name prefix (i.e.: wgt_name1 - wgt_name100)
  wgts <- get_wgt_names(wgt_name)
  
  if(agg == 'count') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, sum), by = mget(factors), .SDcols = wgts]
    
  } else if(agg == 'sum') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD*get(agg_var), sum), by = mget(factors), .SDcols = wgts]
    
  } else if(agg == 'avg') {
    
    wgt_freq <- data[eval(parse(text = subset)), lapply(.SD, function(x) sum(x*get(agg_var))/sum(x)), by = mget(factors), .SDcols = wgts]
    
  } else if(agg == 'avg_trip_rate') {
    
    group <- c('HOUSEID', 'PERSONID', factors)
    
    trp_counts <- dt[eval(parse(text = subset)), .(trps = .N), by = group]
    
    setkeyv(trp_counts,group)
    setkeyv(dt,group)
    
    dt <- trp_counts[unique(dt[, c(group, wgts), with = FALSE]), nomatch=0]
    
    wgt_freq <- dt[, lapply(.SD, function(x) sum(x*trps)/sum(x)), by = mget(factors), .SDcols = wgts]
    
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

