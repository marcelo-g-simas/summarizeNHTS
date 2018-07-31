#' @title Make a formatted flat contingency table.
#' 
#' @description
#' Creates a formatted flat contingency table with class \link[stats]{ftable}.
#' 
#' @param tbl A data.table returned by \link[summarizeNHTS]{summarize_data}.
#' @param output A named character vector denoting the output.
#' @param col_level_threshold Maximum amount of group variable levels in the column position.
#' @param row_vars Group variables to be represented in the row position.
#' @param col_vars Group variables to be represented in the column position.
#' @param samp_size_warn logical. Attach asterisk to value if sample size is less than 30.
#' @param ... Optional formatting arguments. See \link[summarizeNHTS]{format_values}.
#' 
#' @export
make_crosstab <- function(tbl, output = crosstab_output(tbl), col_level_threshold = 8, 
                          row_vars = NULL, col_vars = NULL, samp_size_warn = F,...) {
  
  if (!'HTS.summary.table' %in% class(tbl)) {
    stop('tbl argument is not an "HTS.summary.table" object (returned by the summarize_data function).')
  }
  
  by <- attr(tbl,'by')
  response <- attr(tbl,'response')
  agg_var <- attr(tbl,'agg_var')
  agg_label <- attr(tbl,'agg_label')
  error <- attr(tbl,'error')
  prop <- attr(tbl, 'prop')
  
  # Combine Aggregate variable name with label for numeric aggregates
  agg_label <- paste(agg_label, agg_var)
  
  if(length(by) == 0) {
    
    formatted_tbl <- tbl[, lapply(.SD, format_values)]
    xtbl <- t(as.table(t(formatted_tbl)))
    row.names(xtbl) <- ''
    colnames(xtbl) <- output
    names(dimnames(xtbl)) <- c('', agg_label)
    ftbl <- ftable(xtbl)
    return(ftbl)
    
  } else if(length(by) > 3) {
    warning('Cannot build table with more than 3 group variables')
    return()
  }
  
  if(length(output) == 1) {
    #A new table dimension is not created when one output statistic is specified 
    output_dimension <- 'rep(output, nrow(tbl))'
  } else output_dimension <- NULL
  
  # Construct formula for xtabs cross tabulation
  response_combined <- paste0('cbind(',paste(names(output), collapse = ','),')')
  f <- as.formula(paste(response_combined, paste(c(by, output_dimension), collapse = '+'), sep = '~'))

  # Create xtabs table object
  xtbl <- xtabs(formula = f, data = tbl, exclude = NULL, na.action=na.pass, drop.unused.levels = T)
  if(any(names(dimnames(xtbl)) == '')) {
    names(dimnames(xtbl))[names(dimnames(xtbl)) == ''] <- agg_label
    dimnames(xtbl)[[agg_label]] <- output
  } else {
    names(dimnames(xtbl))[names(dimnames(xtbl)) == output_dimension] <- agg_label
  }

  # If row_vars/col_vars parameters are not specified, then programmatically define them
  if(is.null(row_vars) & is.null(col_vars)) {
    # List of xtable dimensions
    xtbl_dim <- lapply(attr(xtbl, "dimnames"), length)

    #If there are less than 4 table dimensions
    if(length(xtbl_dim) < 4) {
      # Dimensions greater than the threshold are the row_vars
      row_vars <- names(xtbl_dim[xtbl_dim > col_level_threshold])
      # If no row_vars greater then the threshold, then the largest dimension is the row_var
      if(length(row_vars) == 0) row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1]
    } else {
      # With 4 or more table dimensions, the two largest dimensions are the row_vars
      xtbl_dim <- xtbl_dim[names(xtbl_dim) != agg_label]
      row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1:2]
    }
  }
  
  # If percentage argument is not passed, set the default
  format_arguments <- list(...)
  if(!'percentage' %in% names(format_arguments)) {
    format_arguments <- c(format_arguments, percentage = prop)
  }
  
  # Format table
  xtbl <- tapply(X = xtbl, INDEX = expand.grid(dimnames(xtbl)), FUN = function(x) {
    do.call(format_values, c(list(x = x), format_arguments))
  })
  
  # Create flat contingency table object
  ftbl <- ftable(xtbl, row.vars = row_vars, col.vars = col_vars, exclude = NULL)
  
  # Warning symbol "*" next to cells with low sample sizes (n < 30)
  if(samp_size_warn == T) {
    N_response <- paste0('cbind(',paste(rep('N', length(output)), collapse = ','),')')
    N_f <- as.formula(paste(N_response, paste(c(by, output_dimension), collapse = '+'), sep = '~'))
    N_tbl <- xtabs(formula = N_f, data = tbl, exclude = NULL, na.action=na.pass, drop.unused.levels = T)
    N_ftbl <- ftable(N_tbl, row.vars = row_vars, col.vars = col_vars)
    sml_smp <- apply(N_ftbl, 1:2, function(x) x < 30)
    ftbl[sml_smp] <- paste(ftbl[sml_smp],'*')
  }

  return(ftbl)

}
