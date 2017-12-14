library(data.table)
library(R6)

#' @importFrom R6 R6Class
HTS.codebook <- R6Class("HTS.codebook",
  lock_objects = FALSE,
  public = list(
    print = function(...) {
      cat("<HTS.codebook>")
      cat(sprintf('\nDataset: %s', self$dataset))
    },
    initialize = function(dataset = NULL, variables, values) {
      self$dataset <- dataset
      self$variables <- variables
      self$values <- values
    },
    dataset = character(),
    variables = data.table(),
    values = data.table()
  )
)

# 2001
variables_2001 <- fread('prep/codebook/2001/variables.csv', colClasses = rep('character',4))
values_2001 <- fread('prep/codebook/2001/values.csv', colClasses = rep('character',3))
codebook_2001 <- HTS.codebook$new('2001', variables_2001, values_2001)

# 2009
variables_2009 <- fread('prep/codebook/2009/variables.csv', colClasses = rep('character',4))
values_2009 <- fread('prep/codebook/2009/values.csv', colClasses = rep('character',3))
codebook_2009 <- HTS.codebook$new('2009', variables_2009, values_2009)

# 2017
variables_2017 <- fread('prep/codebook/2017/variables.csv', colClasses = rep('character',4))
values_2017 <- fread('prep/codebook/2017/values.csv', colClasses = rep('character',3))
codebook_2017 <- HTS.codebook$new('2017', variables_2017, values_2017)

# Write to package data folder
devtools::use_data(codebook_2001, codebook_2009, codebook_2017, overwrite = T)

