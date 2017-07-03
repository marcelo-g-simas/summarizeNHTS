#' @export
download_nhts_data <- function(dataset, exdir = getwd()) {

  if(!dataset %in% c('2001','2009')) stop(dataset, ' is not a valid NHTS data set.')
  
  new_path <- file.path(exdir,'csv',dataset)
  
  question <- paste0('You are about to download large data files to the directory below. Continue? (y/n):  \n',new_path,'\n')
  answer <- readline(question)
  
  if(tolower(answer) == 'n') {
    cat('Exiting function.\nIf you would like to specify a directory, do so in the "exdir" parameter.\nFor more information, run ?download_nhts_2009\n')
    return()
  } else if(!tolower(answer) %in% c('y','n')) {
    cat(dQuote(answer),' is not a valid answer choice. Please type “y” or “n”.')
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

  } else stop(dataset, ' is not a valid NHTS data set.')

  unzip(temp_data, junkpaths = TRUE, exdir = new_path)
  unzip(temp_wgts, junkpaths = TRUE, exdir = new_path)

  unlink(c(temp_data, temp_wgts))
}
