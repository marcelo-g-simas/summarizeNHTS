#' @title Make a formatted flat contingency table.
#' 
#' @description
#' Creates a formatted flat contingency table with class \link[stats]{ftable}.
#' 
#' @param tbl A data.table returned by \link[summarizeNHTS]{make_table}.
#' @param output A named character vector denoting the output.
#' @param col_level_threshold Maximum amount of factor levels in the column position.
#' @param row_vars Factors to be represented in the row position.
#' @param col_vars Factors to be represented in the column position.
#' @param samp_size_warn logical. Attach asterisk to value if sample size is less than 30.
#' @param digits integer. Number of significant digits to use.
#' @param percentage logical. Treat proportions as percentages?
#' @param scientific logical. Use scientific notation for number formatting?
#' @param multiplier numeric. A multiplier to use for numeric value display (i.e. For "In Thousands", use multiplier = 1000)
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
  
  if(length(output) == 1) {
    #A new table dimension is not created when one output statistic is specified 
    output_dimension <- 'rep(output, nrow(tbl))'
  } else output_dimension <- NULL
  
  # Construct formula for xtabs cross tabulation
  response_combined <- paste0('cbind(',paste(names(output), collapse = ','),')')
  f <- as.formula(paste(response_combined, paste(c(factors, output_dimension), collapse = '+'), sep = '~'))

  # Create xtabs table object
  xtbl <- xtabs(formula = f, data = tbl, exclude = NULL, na.action=na.pass)
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

  # Format table
  xtbl <- tapply(X = xtbl, INDEX = expand.grid(dimnames(xtbl)), FUN = format_values, 
    digits = digits, percentage = percentage, scientific = scientific, multiplier = multiplier
  )
  
  # Create flat contingency table object
  ftbl <- ftable(xtbl, row.vars = row_vars, col.vars = col_vars, exclude = NULL)
  
  # Warning symbol "*" next to cells with low sample sizes (n < 30)
  if(samp_size_warn == T) {
    N_response <- paste0('cbind(',paste(rep('N', length(output)), collapse = ','),')')
    N_f <- as.formula(paste(N_response, paste(c(factors, output_dimension), collapse = '+'), sep = '~'))
    N_tbl <- xtabs(formula = N_f, data = tbl, exclude = NULL, na.action=na.pass)
    N_ftbl <- ftable(N_tbl, row.vars = row_vars, col.vars = col_vars)
    sml_smp <- apply(N_ftbl, 1:2, function(x) x < 30)
    ftbl[sml_smp] <- paste(ftbl[sml_smp],'*')
  }

  return(ftbl)

}
