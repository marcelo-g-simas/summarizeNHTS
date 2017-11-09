library(summarizeNHTS)

CODEBOOK <- setRefClass("CODEBOOK", fields = list(labels = 'data.table', variables = 'data.table'))
# CODEBOOK$methods(update_variables = function(v) variables <<- v)
# CODEBOOK$methods(update_labels = function(l) labels <<- l)

codebook_2001 <- CODEBOOK$new(labels = nhts_2001$labels, variables = nhts_2001$variables)
codebook_2009 <- CODEBOOK$new(labels = nhts_2009$labels, variables = nhts_2009$variables)
codebook_2017 <- CODEBOOK$new(labels = nhts_2016$labels, variables = nhts_2016$variables)

devtools::use_data(codebook_2001, codebook_2009, codebook_2017, overwrite = T)

