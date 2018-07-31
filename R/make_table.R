#' @title Make HTML Table
#' 
#' @description Make formatted HTML tables utilizing \link[htmlTable]{htmlTable}
#' 
#' @param tbl An data.table object returned by \link[summarizeNHTS]{summarize_data}
#' @param title Title of the table
#' @param confidence Confidence level for margin of error calculation. Defaults to 0.95. Set to NULL for standard error.
#' @param ... Other arguments passed to \link[summarizeNHTS]{make_crosstab}. 
#' Includes formatting arguments. See \link[summarizeNHTS]{format_values}.
#' 
#' @import htmlTable
#' @export
make_table <- function(tbl, title = '', confidence = 0.95, ...) {
  
  # Apply margin of error
  tbl <- use_moe(tbl, confidence)
  
  # Create flat contingency table
  ftbl <- make_crosstab(tbl, ...)
  
  agg_label <- attr(tbl, 'agg_label')
  row.vars <- attr(ftbl,'row.vars')
  col.vars <- attr(ftbl,'col.vars')

  # Row Configuration
  if(length(row.vars) == 0) {
    rnames <- ''
    rowlabel <- names(col.vars[1])
    rgroup <- NULL
    n.rgroup <- NULL
  } else if(length(row.vars) == 1) {
    rnames <- row.vars[[1]]
    rowlabel <- ''
    rgroup <- names(row.vars)
    if (!is.null(rgroup)) {
      n.rgroup <- length(row.vars[[1]])
    } else {
      n.rgroup <- NULL
    }
  } else if(length(row.vars) == 2) {
    rnames <- rep(row.vars[[2]], length(row.vars[[1]]))
    rowlabel <- paste(names(row.vars), collapse = '<br><i>by</i><br>')
    rgroup <- row.vars[[1]]
    n.rgroup <- rep(length(row.vars[[2]]), length(row.vars[[1]]))
  } else {
    stop('Cannot specify more than 2 variables in the row position.\n',
         'row_vars requires 2 variables for tables with 3 group variables.')
  }

  # Column Configuration
  if(length(col.vars) == 1) {
    header <- col.vars[[1]]
    cgroup <- names(col.vars)
    n.cgroup <- length(header)
  } else if (length(col.vars) == 2) {
    header <- rep(col.vars[[2]], length(col.vars[[1]]))
    cgroup <- rbind(NA, col.vars[[1]])
    cgroup[1,1] <- names(col.vars[1])
    n.cgroup <- array(NA, dim = dim(cgroup))
    n.cgroup[1,1] <- length(header)
    n.cgroup[2, ] <- length(col.vars[[2]])
  } else {
    stop('Cannot specify more than 2 variables in the column position.\n',
         'col_vars requires 2 variables for tables with 3 group variables.')
  }
  
  # css.cell Configurations
  if(length(unlist(col.vars)) == 1) {
    # Possible htmlTable Bug: Does not respect css.cell matrix when only 1 column
    css.cell <- "border-left: .5px solid #e0e0e0; padding: 4px; font-size: 12;"
  } else {
    css.cell <- rbind(
      rep("background: #D7E5EF; padding-left: .5em; padding-right: .2em;", times=ncol(ftbl)),
      matrix("border-left: .5px solid #e0e0e0; padding: 4px; font-size: 12;", ncol=ncol(ftbl), nrow=nrow(ftbl))
    )
  }

  # Create htmlTable
  html_table <- htmlTable(
    ftbl,
    caption = title,
    rnames = rnames,
    rowlabel = rowlabel,
    rgroup = rgroup,
    n.rgroup = n.rgroup,
    header = header,
    cgroup = cgroup,
    n.cgroup = n.cgroup,
    css.cell = css.cell,
    css.rgroup.sep = "border-top: .5px solid #cccccc;",
    col.columns = c('#f7f9fb','none'),
    padding.rgroup = paste(rep('&nbsp;',8), collapse = ''),
    css.table = "margin-top: 1em; margin-bottom: 1em; font-family: calibri; font-size: 13;"
  )
  
  return(html_table)
  
}
