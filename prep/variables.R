#Read variable meta data from codebook browser
library(rvest)

url <- "http://nhts.ornl.gov/tables09/CodebookBrowser.aspx"
variables <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="tableOne"]') %>%
  html_table( fill = TRUE)
variables <- na.omit(data.table(variables[[1]][,-1]))

data_files <- c("Household File","Person File","Vehicle File","Daytrip File")
variables[, (data_files) := lapply(.SD, function(x) {x == 'Y'}), .SDcols = data_files]
variables[, Levels := apply(.SD , 1, function(x) {if(x[1]) 'Household' else if(x[2]) 'Person' else if(x[4]) 'Trip' else 'Vehicle'}), .SDcols = data_files]

write.csv(variables, './data/2009/variables.csv', row.names = F)



