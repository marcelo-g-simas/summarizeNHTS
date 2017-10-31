library(data.table)
# variables_2001 <- summarizeNHTS::nhts_2001$variables
# variables_2009 <- summarizeNHTS::nhts_2009$variables

# Rename
setnames(
  x = variables_2001,
  old = c('Variable','Levels','Description','Type'), 
  new = c('DELIVERY_NAME','DELIVERY_TABLE_NAME','DELIVERY_LABEL','DATA_TYPE')
)
setnames(
  x = variables_2009,
  old = c('Variable','Levels','Description','Type'), 
  new = c('DELIVERY_NAME','DELIVERY_TABLE_NAME','DELIVERY_LABEL','DATA_TYPE')
)

# Lower Case table names
variables_2001$DELIVERY_TABLE_NAME <- tolower(variables_2001$DELIVERY_TABLE_NAME)
variables_2009$DELIVERY_TABLE_NAME <- tolower(variables_2009$DELIVERY_TABLE_NAME)

variables_2001 <- variables_2001[, list(DELIVERY_NAME, DELIVERY_TABLE_NAME, DELIVERY_LABEL, DATA_TYPE)]
variables_2009 <- variables_2009[, list(DELIVERY_NAME, DELIVERY_TABLE_NAME, DELIVERY_LABEL, DATA_TYPE)]

#==========================================================================================#

labels_2001 <- summarizeNHTS::nhts_2001$labels
labels_2009 <- summarizeNHTS::nhts_2009$labels

colnames(labels_2001) <- c('NAME','VALUE','DESCRIPTION')
colnames(labels_2009) <- c('NAME','VALUE','DESCRIPTION')

nhts_2001 <- list(
  labels = labels_2001,
  variables = variables_2001
)

nhts_2009 <- list(
  labels = labels_2009,
  variables = variables_2009
)



devtools::use_data(nhts_2001, nhts_2009, overwrite = T)



