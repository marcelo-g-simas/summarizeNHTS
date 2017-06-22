#' @export
download_nhts_data <- function(dataset, exdir = getwd()) {

  new_path <- file.path(exdir,'csv',dataset)

  if(length(list.files(new_path, '.csv', ignore.case = T)) > 0) {
    warning("The directory\n", new_path, "\nalready exists and contains CSV's. Stopping download process.")
    return()
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

  warning('Creating new directory to house NHTS data: \n',new_path)

  unzip(temp_data, junkpaths = TRUE, exdir = new_path)
  unzip(temp_wgts, junkpaths = TRUE, exdir = new_path)

  unlink(c(temp_data, temp_wgts))
}
