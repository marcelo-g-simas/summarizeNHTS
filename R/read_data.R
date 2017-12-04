#' @title Read HTS data.
#' 
#' @description Read and merge NHTS data for analysis.
#' 
#' @param dataset The study year of the dataset.
#' @param select A character vector of NHTS variable names to select for analysis. Defaults to all.
#' @param csv_path The parent directory of "/csv/dataset/". Defaults to working directory.
#' 
#' @details 
#' \code{read_data} is a wrapper for reading in variables from the correct csvs
#' and merging the corresponding tables on correct keys. \link[data.table]{fread} and
#' \link[data.table]{merge.data.table} are used for performance benefits.
#' 
#' @export
#' @import data.table
#' 
read_data <- function(dataset, select = select_all(dataset), csv_path = getwd()) {
  hts_obj <- HTS.data$new(dataset, select, csv_path)
  hts_obj$read_all()
  return(hts_obj)
}

#' @importFrom R6 R6Class
HTS.data <- R6Class("HTS.data",
  public = list(
    initialize = function(dataset = NULL, select = NULL, path = NULL) {
      self$dataset <- dataset
      self$select <- select
      self$path <- paste(file.path(path, "csv"), dataset, sep = '/')
    },
    print = function(...) {
      cat("<HTS.data> Environment")
      cat(sprintf('\nDataset: %s', self$dataset))
    },
    dataset = NULL,
    select = NULL,
    path = NULL,
    data = list(
      trip = NULL,
      person = NULL,
      household = NULL,
      vehicle = NULL
    ),
    weights = list(
      household = NULL,
      person = NULL,
      trip_keys = NULL
    ),
    read_all = function() {
      private$path_check()
      cat('=========================================================================\n')
      cat('Reading Household Table.\n')
      self$data$household <- private$read_data('household')
      cat('=========================================================================\n')
      cat('Reading Person Table.\n')
      self$data$person <- private$read_data('person')
      cat('=========================================================================\n')
      cat('Reading Trip Table.\n')
      self$data$trip <- private$read_data('trip')
      cat('=========================================================================\n')
      cat('Reading Vehicle Table.\n')
      self$data$vehicle <- private$read_data('vehicle')
      cat('=========================================================================\n')
      cat('Reading Household Weights\n')
      self$weights$household <- private$read_weights('household')
      cat('=========================================================================\n')
      cat('Reading Person Weights\n')
      self$weights$person <- private$read_weights('person')
      cat('=========================================================================\n')
      self$weights$trip_keys <- self$data$trip[, mget(get_table_keys('trip'))]
    }
  ),
  private = list(
    path_check = function() {
      if(length(list.files(self$path, '.csv', ignore.case = T)) == 0) {
        stop(
          "\nThe directory below does not exist or does not contain csv files.\n", self$path,
          "\n- Make sure the correct path is specified.",
          "\n- Or run ?download_nhts_data if you have not yet downloaded the data."
        )
      }
    },
    variable_lookup = function() {
      variables <- CB(self$dataset)$variables
      select_match <- match(self$select, variables$NAME)
      variables_selected <- variables[select_match]
      return(variables_selected)
    },
    variable_classes = function(table_name) {
      variables_selected <- private$variable_lookup()
      variables_table <- variables_selected[TABLE == table_name]
      col_classes <- variables_table[, TYPE]
      names(col_classes) <- variables_table[, NAME]
      return(col_classes)
    },
    read_data = function(table_name) {
      table_csv <- sprintf('%s.csv', table_name)
      variables_selected <- private$variable_lookup()
      vars <- variables_selected[TABLE == table_name, NAME]
      table_key <- get_table_keys(table_name)
      key_classes <- rep("character", length(table_key))
      names(key_classes) <- table_key
      fread(
        input = file.path(self$path, table_csv), 
        select = c(get_table_keys(table_name), vars), 
        key = table_key,
        colClasses = c(key_classes, private$variable_classes(table_name))
      )
    },
    read_weights = function(table_name) {
      table_csv <- sprintf('%s_weights.csv', table_name)
      table_key <- get_table_keys(table_name)
      key_classes <- rep("character", length(table_key))
      names(key_classes) <- table_key
      fread(
        input = file.path(self$path, table_csv), 
        select = c(table_key, WT(table_name, self$dataset)), 
        key = table_key,
        colClasses = key_classes
      )
    }
  )
)

