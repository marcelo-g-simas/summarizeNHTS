#' @title Read HTS data and weights.
#' 
#' @param dataset The study year of the dataset. Currently supports "2001" and "2009".
#' @param select A character vector of NHTS variable names to select for analysis. Defaults to all variables in the codebook.
#' @param csv_path The parent directory of "/csv/dataset/". Defaults to working directory.
#' 
#' @details 
#' \code{read_data} is a wrapper for reading in variables from the correct csvs
#' and merging the corresponding tables on correct keys. \link[data.table]{fread} and
#' \link[data.table]{merge} are used for performance benefits.
#' 
#' @return read_data returns an object of class "HTS.data". It contains the data files and weights necessary for
#' querying the NHTS dataset, used primarily by \link[summarizeNHTS]{summarize_data}.
#' 
#' The "HTS.data" object is essentially a list of data.tables broken up by data and weights.
#' 
#' Accessing the data:
#' \itemize{
#'   \item \strong{household} - Household data file
#'   \item \strong{person} - Person data file
#'   \item \strong{trip} - Trip data file
#'   \item \strong{vehicle} - Vehicle data file
#' }
#' 
#' Accessing the weights:
#' \itemize{
#'   \item \strong{household} - Household weight file. Used for weighting household and vehicle data.
#'   \item \strong{person} - Person weight file. Also includes trip weights at the person level.
#' }
#' 
#' @examples
#' \donttest{
#' # Read 2009 NHTS data with specified csv path:
#' nhts_data <- read_data('2009', csv_path = 'C:/NHTS')
#' 
#' # Access the data
#' nhts_data$data$household     # household data
#' nhts_data$data$person        # person data
#' nhts_data$data$trip          # trip data
#' nhts_data$data$vehicle       # vehicle data
#' 
#' # Access the weights
#' nhts_data$weights$household  # household weights
#' nhts_data$weights$person     # person and trip weights
#' }
#' 
#' @export
#' @import data.table
#' 
read_data <- function(dataset, csv_path = getwd(), select = select_all(dataset)) {
  hts_obj <- HTS.data$new(dataset, select, csv_path)
  hts_obj$read_all()
  
  # Add derived variables to codebook and dataset if config csv exists
  derived_variable_config <- file.path(hts_obj$path, 'derived_variable_config.csv')
  if (file.exists(derived_variable_config)) {
    derived_variables(hts_obj, derived_variable_config)
  }
  
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
      message('=========================================================================')
      message('Reading Person Weights')
      self$weights$person <- private$read_weights('person')
      message('=========================================================================')
      message('Reading Household Weights')
      self$weights$household <- private$read_weights('household')
      message('=========================================================================')
      message('Reading Trip Table.')
      self$data$trip <- private$read_data('trip')
      message('=========================================================================')
      message('Reading Person Table.')
      self$data$person <- private$read_data('person')
      message('=========================================================================')
      message('Reading Household Table.')
      self$data$household <- private$read_data('household')
      message('=========================================================================')
      message('Reading Vehicle Table.')
      self$data$vehicle <- private$read_data('vehicle')
      message('=========================================================================')
      #===============================================================================#
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
    table_variables = function(table_name) {
      table_csv <- sprintf('%s.csv', table_name)
      names(fread(file.path(self$path, table_csv), nrows = 0))
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
      col_classes <- variables_table[NAME %in% private$table_variables(table_name), TYPE]
      names(col_classes) <- variables_table[NAME %in% private$table_variables(table_name), NAME]
      return(col_classes)
    },
    read_data = function(table_name) {
      table_csv <- sprintf('%s.csv', table_name)
      variables_selected <- private$variable_lookup()
      vars <- variables_selected[TABLE == table_name, NAME]
      vars <- vars[vars %in% private$table_variables(table_name)]
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
      weight_names <- WT(table_name, self$dataset)
      wts <- fread(
        input = file.path(self$path, table_csv), 
        select = c(table_key, weight_names), 
        key = table_key,
        colClasses = key_classes
      )
      wgt_names <- WT(table_name, self$dataset)
      wts[, (weight_names) := lapply(.SD, as.numeric), .SDcols = weight_names]
    }
  )
)

