library(data.table)
variables_2001 <- summarizeNHTS::nhts_2001$variables
variables_2009 <- summarizeNHTS::nhts_2009$variables

setnames(variables_2001,c('Variable','Levels','Description'), c('DELIVERY_NAME','DELIVERY_TABLE_NAME','DELIVERY_LABEL'))
setnames(variables_2009,c('Variable','Levels','Description'), c('DELIVERY_NAME','DELIVERY_TABLE_NAME','DELIVERY_LABEL'))

variables_2001$DELIVERY_TABLE_NAME <- tolower(variables_2001$DELIVERY_TABLE_NAME)
variables_2009$DELIVERY_TABLE_NAME <- tolower(variables_2009$DELIVERY_TABLE_NAME)

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

nhts_2001$variables <- nhts_2001$variables[, list(DELIVERY_NAME, DELIVERY_TABLE_NAME, DELIVERY_LABEL)]
nhts_2009$variables <- nhts_2009$variables[, list(DELIVERY_NAME, DELIVERY_TABLE_NAME, DELIVERY_LABEL)]

devtools::use_data(nhts_2001, nhts_2009, overwrite = T)



