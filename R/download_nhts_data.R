#' @title Download NHTS datasets.
#' 
#' @description Download public use NHTS datasets from the web.
#' 
#' @param dataset The study year of the dataset to download. Currently either '2001' or '2009'.
#' @param exdir The directory to house the data. Defaults to working directory.
#' 
#' @details
#' We recommend that you first create an \href{https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects}{RStudio Project}.
#' This will ensure that the data is downloaded under your project directory and will be helpful for future package utilization.
#' Note: A "/csv/dataset/" directory will always be created under exdir for organizational purposes.
#' 
#' @examples
#' \donttest{
#' # Download the 2009 NHTS data to specified directory:
#' download_nhts_data("2009", "C:/NHTS")
#' }
#' 
#' @export
download_nhts_data <- function(dataset, exdir = getwd()) {
  
  if(!dataset %in% c('2001','2009','2017')) stop(dataset, ' is not a valid NHTS data set.')
  
  new_path <- file.path(exdir,'csv',dataset)
  
  question <- paste0('You are about to download large data files to the directory below. Continue? (y/n):  \n',new_path,'\n')
  answer <- readline(question)
  
  if(tolower(answer) == 'n') {
    cat('Exiting function.\nIf you would like to specify a directory, do so in the "exdir" parameter.\nFor more information, run ?download_nhts_2009\n')
    return()
  } else if(!tolower(answer) %in% c('y','n')) {
    cat('"',answer,'" is not a valid answer choice. Please type "y" or "n".', sep = "")
    return(download_nhts_data(dataset,exdir))
  } 
  
  if(length(list.files(new_path, '.csv', ignore.case = T)) > 0) {
    warning("The directory\n", new_path, "\nalready exists and contains CSV's. Stopping download process.")
    return('')
  }
  
  temp_data <- tempfile()
  temp_wgts <- tempfile()
  
  if(dataset == '2001') {
    
    download.file("http://nhts.ornl.gov/2001/download/Ascii.zip", temp_data)
    download.file("http://nhts.ornl.gov/2001/download/replicates_ascii.zip", temp_wgts)
    
  } else if (dataset == '2009') {
    
    download.file("http://nhts.ornl.gov/2009/download/Ascii.zip", temp_data)
    download.file("http://nhts.ornl.gov/2009/download/ReplicatesASCII.zip", temp_wgts)
    
  } else if (dataset == '2017') {
    
    download.file("http://nhts.ornl.gov/assets/2016/download/Csv.zip", temp_data)
    download.file("http://nhts.ornl.gov/assets/2016/download/ReplicatesCSV.zip", temp_wgts)
    
  } else stop(dataset, ' is not a valid NHTS data set.')
  
  unzip(temp_data, junkpaths = TRUE, exdir = new_path)
  unzip(temp_wgts, junkpaths = TRUE, exdir = new_path)
  
  unlink(c(temp_data, temp_wgts))
  
  # Standardize file names
  #-------------------------------------------------------------#
  household_path <- file.path(new_path,'household.csv')
  person_path <- file.path(new_path,'person.csv')
  vehicle_path <- file.path(new_path,'vehicle.csv')
  trip_path <- file.path(new_path,'trip.csv')
  household_weights_path <- file.path(new_path,'household_weights.csv')
  person_weights_path <- file.path(new_path,'person_weights.csv')
  #-------------------------------------------------------------#
  file.rename(file.path(new_path,'HHV2PUB.CSV'), household_path)
  file.rename(file.path(new_path,'HHPUB.CSV'), household_path)
  file.rename(file.path(new_path,'PERV2PUB.CSV'), person_path)
  file.rename(file.path(new_path,'PERPUB.CSV'), person_path)
  file.rename(file.path(new_path,'VEHV2PUB.CSV'), vehicle_path)
  file.rename(file.path(new_path,'VEHPUB.CSV'), vehicle_path)
  file.rename(file.path(new_path,'DAYV2PUB.CSV'), trip_path)
  file.rename(file.path(new_path,'DAYPUB.CSV'), trip_path)
  file.rename(file.path(new_path,'trippub.csv'), trip_path)
  file.rename(file.path(new_path,'hh50wt.csv'), household_weights_path)
  file.rename(file.path(new_path,'hhwgt.csv'), household_weights_path)
  file.rename(file.path(new_path,'per50wt.csv'), person_weights_path)
  file.rename(file.path(new_path,'pr50wt.csv'), person_weights_path)
  file.rename(file.path(new_path,'perwgt.csv'), person_weights_path)
  #-------------------------------------------------------------#
  
  # Copy derived variable config template from package directory to new csv path
  package_dir <- find.package('summarizeNHTS')
  derived_variable_config_csv <- file.path(package_dir, 'derived_variables/derived_variable_config.csv')
  file.copy(derived_variable_config_csv, new_path)
  
  # 2001 public use datset, ID9 should be HOUSEID
  if (dataset == '2001') {
    hh <- readLines(household_weights_path)
    hh[1] <- sub('ID9','HOUSEID', hh[1])
    writeLines(hh, household_weights_path)
    rm(hh)
    per <- readLines(person_weights_path)
    per[1] <- sub('ID9','HOUSEID', per[1])
    writeLines(per, person_weights_path)
    rm(per)
  }
  
}