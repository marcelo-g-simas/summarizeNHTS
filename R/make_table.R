#' @title Make HTML Table
#' 
#' @description Make formatted HTML tables utilizing \link[htmlTable]{htmlTable}
#' 
#' @param tbl An data.table object returned by \link[summarizeNHTS]{make_table}
#' @param title Title of the table
#' @param ... Other arguments passed to \link[summarizeNHTS]{make_crosstab}
#' 
#' @importFrom htmltools html_print
#' @import htmlTable
#' @export
make_table <- function(tbl, title = '', ...) {

  ftbl <- make_crosstab(tbl, ...)
  
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
    n.rgroup <- length(row.vars[[1]])
  } else {
    rnames <- rep(row.vars[[2]], length(row.vars[[1]]))
    rowlabel <- paste(names(row.vars), collapse = '<br><i>by</i><br>')
    rgroup <- row.vars[[1]]
    n.rgroup <- rep(length(row.vars[[2]]), length(row.vars[[1]]))
  }

  # Column Configuration
  if(length(col.vars) == 1) {
    header <- col.vars[[1]]
    cgroup <- ''
    n.cgroup <- NULL
  } else {
    header <- rep(col.vars[[2]], length(col.vars[[1]]))
    cgroup <- col.vars[[1]]
    n.cgroup <- rep(length(col.vars[[2]]),length(col.vars[[1]]))
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
  
  html_print(html_table)

}
