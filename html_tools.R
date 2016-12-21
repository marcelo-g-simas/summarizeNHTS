################################################################################################
# make_html_table                                                                              #
################################################################################################

make_html_table <- function(m) {
  
  innerHtmlTable <- function(x) {
    x <- paste0('<td class="htsTableCell">',trimws(x),'</td>')
    x[1] <- paste0('<tr class="htsTableRow">',x[1])
    x[length(x)] <- paste0(x[length(x)],'</tr>')
    paste(x,collapse = '')
  }
  
  x <- paste0(
    '<table class="htsTable">',
    paste0(apply(m,1,innerHtmlTable),collapse = ''),
    '</table>'
  )  
  
  cat(x)
}
