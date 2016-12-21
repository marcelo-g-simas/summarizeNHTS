
################################################################################################
# make_contingency_table                                                                       #
################################################################################################

make_contingency_table <- function(tbl, show_variance = TRUE) {
  
  response <- attr(tbl,'response')
  factors <- attr(tbl,'factors')
  variance <- attr(tbl,'variance')
  
  if(show_variance == TRUE) response <- paste0('cbind(',response,',',variance,')')
  
  f <- as.formula(paste(response,paste(factors, collapse = '+'), sep = '~'))
  
  xtbl <- xtabs(formula = f, data = tbl)
  
  ftable(xtbl)
  
}


################################################################################################
# add_totals                                                                                   #
################################################################################################

add_totals <- function(x,margin) {
  
  tbl <- as.table(x)
  tbl <- addmargins(tbl,margin)
  ftable(tbl)
  
}


################################################################################################
# format_table                                                                                 #
################################################################################################

format_table <- function(x, percentage = FALSE) {
  
  #Numeric formatting:
  x <- round(x, digits = 4)
  
  #Format Percentages:
  if(percentage == TRUE) {
    x <- ftable(
      apply(x,1:2,function(cell) {
        paste0(100*cell,'%')
      })
    )
  }      
  
  #Format into character matrix for makeHtmlTable function
  x <- format(x, quote = FALSE, scientific = FALSE)
  x
}

