#' Creates a formatted flat contingency table (\link[stats]{ftable}).
#' 
#' @param tbl A data.table returned by \link[NHTS.summarizer]{make_table}.
#' @param col_level_threshold Maximum amount of factor levels in the column position.
#' @param row_vars Factors to be represented in the row position.
#' @param col_vars Factors to be represented in the column position.
#' @param round_digits Number of digits to round to. Passed to digits parameter in \link[base]{round}.
#' 
#' @export
make_crosstab <- function(tbl, col_level_threshold = 8, row_vars = NULL, col_vars = NULL, round_digits = 2) {

  factors <- attr(tbl,'factors')
  response <- attr(tbl,'response')
  variance <- attr(tbl,'variance')

  if(is.null(factors)) {
    warning('Cannot build table with NULL factors.')
    return()
  } else if(length(factors) > 3) {
    warning('Cannot build table with more than 3 factors.')
    return()
  }

  # Construct formula for xtabs cross tabulation
  response_combined <- paste0('cbind(',response,',',variance,')')
  f <- as.formula(paste(response_combined,paste(factors, collapse = '+'), sep = '~'))

  # Create xtabs table object
  xtbl <- xtabs(formula = f, data = tbl)

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
      xtbl_dim <- xtbl_dim[names(xtbl_dim) != ""]
      row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1:2]
    }
  }

  # Configure digits/rounding/scientific notation
  if(max(xtbl) >= 100000) {
    xtbl <- formatC(xtbl, format="E", digits = 2)
    warning("Max number is greater than 100,000. Ignoring rounding and using Scientific Notation")
  } else {
    xtbl <- round(xtbl, round_digits)
  }

  # Create flat contingency table object
  ftbl <- ftable(xtbl, row.vars = row_vars, col.vars = col_vars)

  return(ftbl)

}
