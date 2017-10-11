#' @title Make a formatted flat contingency table.
#' 
#' @description
#' Creates a formatted flat contingency table with class \link[stats]{ftable}.
#' 
#' @param tbl A data.table returned by \link[summarizeNHTS]{make_table}.
#' @param col_level_threshold Maximum amount of factor levels in the column position.
#' @param row_vars Factors to be represented in the row position.
#' @param col_vars Factors to be represented in the column position.
#' @param round_digits Number of digits to round to. Passed to digits parameter in \link[base]{round}.
#' 
#' @export
make_crosstab <- function(tbl, output = crosstab_output(), col_level_threshold = 8, 
                          row_vars = NULL, col_vars = NULL, samp_size_warn = F,
                          digits = 2, percentage = attr(tbl, 'prop'), scientific = F, multiplier = NULL) {

  factors <- attr(tbl,'factors')
  response <- attr(tbl,'response')
  agg_label <- attr(tbl,'agg_label')
  error <- attr(tbl,'error')
  prop <- attr(tbl, 'prop')
  
  if(length(factors) == 0) {
    
    xtbl <- t(as.table(t(tbl[, mget(names(output))])))
    row.names(xtbl) <- ''
    dimnames(xtbl)[[1]] <- agg_label
    dimnames(xtbl)[[2]] <- output
    ftbl <- ftable(xtbl)
    return(ftbl)
    
  } else if(length(factors) > 3) {
    warning('Cannot build table with more than 3 factors.')
    return()
  }
  
  # Construct formula for xtabs cross tabulation
  response_combined <- paste0('cbind(',paste(names(output), collapse = ','),')')
  f <- as.formula(paste(response_combined, paste(factors, collapse = '+'), sep = '~'))

  # Create xtabs table object
  xtbl <- xtabs(formula = f, data = tbl, exclude = NULL, na.action=na.pass)
  if(any(names(dimnames(xtbl)) == '')) {
    names(dimnames(xtbl))[names(dimnames(xtbl)) == ''] <- agg_label
    dimnames(xtbl)[[agg_label]] <- output
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

  # Format table
  xtbl <- apply(X = xtbl, MARGIN = 1:2, FUN = format_values, 
    digits = digits, percentage = percentage, scientific = scientific, multiplier = multiplier
  )
  
  # Create flat contingency table object
  ftbl <- ftable(xtbl, row.vars = row_vars, col.vars = col_vars, exclude = NULL)
  
  # Warning symbol "*" next to cells with low sample sizes (n < 30)
  if(samp_size_warn == T) {
    N_response <- paste0('cbind(',paste(rep('N', length(output)), collapse = ','),')')
    N_f <- as.formula(paste(N_response, paste(factors, collapse = '+'), sep = '~'))
    N_tbl <- xtabs(formula = N_f, data = tbl, exclude = NULL, na.action=na.pass)
    N_ftbl <- ftable(N_tbl, row.vars = row_vars, col.vars = col_vars)
    sml_smp <- apply(N_ftbl, 1:2, function(x) x < 30)
    ftbl[sml_smp] <- paste(ftbl[sml_smp],'*')
  }

  return(ftbl)

}
