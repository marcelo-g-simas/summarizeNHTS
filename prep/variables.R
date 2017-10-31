#Read variable meta data from codebook browser
library(rvest)

#For 2009 variables...
url <- "http://nhts.ornl.gov/tables09/CodebookBrowser.aspx"
variables_2009 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="tableOne"]') %>%
  html_table( fill = TRUE)
variables_2009 <- na.omit(data.table(variables_2009[[1]][,-1]))

data_files <- c("Household File","Person File","Vehicle File","Daytrip File")
variables_2009[, (data_files) := lapply(.SD, function(x) {x == 'Y'}), .SDcols = data_files]
variables_2009[, Levels := apply(.SD , 1, function(x) {if(x[1]) 'Household' else if(x[2]) 'Person' else if(x[3]) 'Vehicle' else 'Trip'}), .SDcols = data_files]
variables_2009[, Type := ifelse(Type == 'N','numeric','character')]


#For 2001 variables...
# Go to http://nhts.ornl.gov/tables09/CodebookBrowser.aspx
# select 2001 NHTS survey and Copy the table to clipboard
variables_2001 <- read.delim("clipboard", check.names=F)
setDT(variables_2001)
data_files <- c("Household File","Person File","Vehicle File","Daytrip File","Longtrip File")
variables_2001[, (data_files) := lapply(.SD, function(x) {x == 'Y'}), .SDcols = data_files]
variables_2001[, Levels := apply(.SD , 1, function(x) {if(x[1]) 'Household' else if(x[2]) 'Person' else if(x[3]) 'Vehicle' else if(x[4]) 'Trip' else 'Longtrip'}), .SDcols = data_files]
variables_2001[, Codebook := NULL]
variables_2001[, `Longtrip File` := NULL]
variables_2001 <- variables_2001[Levels != 'Longtrip']
variables_2001[, Type := ifelse(Type == 'N','numeric','character')]
