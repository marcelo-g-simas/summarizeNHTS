# ======================= #
# Configuration Functions #
# ======================= #

#==================================================================================================#
# Rcpp Requirement (@useDynLib package_name)
#' @useDynLib summarizeNHTS

#==================================================================================================#
#' @export

ID <- function(level) {
  
  id <- switch(
    EXPR = level,
    household = 'HOUSEID',
    person = 'PERSONID',
    trip = 'TDTRPNUM',
    vehicle = 'VEHID'
  )
  if(is.null(id)) stop(level,' is not a valid ID level.')
  
  return(id)
}

#==================================================================================================#
#' @export

CB <- function(dataset) {
  
  if(!dataset %in% c('2001','2009','2017')) {
    stop(dataset, ' is not a valid dataset.')
  }
  
  cb <- switch(
    EXPR = dataset,
    '2001' = codebook_2001,
    '2009' = codebook_2009,
    '2017' = codebook_2017
  )
  
  return(cb)
}

#==================================================================================================#
#' @export

WT <- function(level, dataset) {

  if (dataset == '2001') {
    wgt <- switch(
      EXPR = level,
      household = 'EXPFLLHH',
      person = 'EXPFLLPR',
      trip = 'EXPFLLTD'
    )
    if(is.null(wgt)) stop(level,' is not a valid weight level.')
    replicates <- paste0(gsub('LL', '', wgt), 1:99)
  } else if (dataset == '2009') {
    wgt <- switch(
      EXPR = level,
      household = 'HHWGT',
      person = 'WTPERFIN',
      trip = 'DAYWGT'
    )
    if(is.null(wgt)) stop(level,' is not a valid weight level.')
    replicates <- paste0(wgt, 1:100)
  } else if (dataset == '2017') {
    wgt <- switch(
      EXPR = level,
      household = 'WTHHFIN',
      person = 'WTPERFIN',
      trip = 'DAYWGT'
    )
    if(is.null(wgt)) stop(level,' is not a valid weight level.')
    replicates <- paste0(wgt, 1:98)
  } else {
    stop(dataset, ' is not a valid dataset.')
  }
  
  return(c(wgt, replicates))
}

#==================================================================================================#
#' @export

jk_se <- function(final_weights, replicate_weights, dataset) {
  
  if (dataset == '2001') {
    
    dif <- sweep(replicate_weights, 1, final_weights) ** 2
    E <- apply(dif, 1, function(x) sqrt((98 / 99) * sum(x)))
    
  } else if (dataset == '2009') {
    
    dif <- sweep(replicate_weights, 1, final_weights) ** 2
    E <- apply(dif, 1, function(x) sqrt((99 / 100) * sum(x)))
    
  } else if (dataset == '2017') {
    
    dif <- sweep(replicate_weights, 1, final_weights) ** 2
    E <- apply(dif * (6 / 7), 1, function(x) sqrt(sum(x)))
    
  }
  
  return(E)
}

#==================================================================================================#

.onLoad <- function(libname, pkgname) {
  
  default_options <- list(
    HTS.format.digits = 2,
    HTS.format.percentage = FALSE,
    HTS.format.scientific = FALSE,
    HTS.format.multiplier = NULL,
    HTS.output.W = 'Weighted',
    HTS.output.U = 'Unweighted',
    HTS.output.E = 'Std. Error',
    HTS.output.N = 'Sample Size'
  )
  
  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if(any(toset)) options(default_options[toset])
  
  invisible()
}

#==================================================================================================#

.onAttach <- function(libname, pkgname) {
  
  if(interactive()) {
    packageStartupMessage("\nsummarizeNHTS ", paste0(packageVersion("summarizeNHTS")))
    packageStartupMessage("Developed by Westat (https://www.westat.com)")
    packageStartupMessage('============================================')
    packageStartupMessage("See GitHub page for more information about the package: ",
                          "https://github.com/Westat-Transportation/summarizeNHTS")
    packageStartupMessage("For more information about the National Household Travel Survey (NHTS): ",
                          "http://nhts.ornl.gov/\n")
  }
  
}


