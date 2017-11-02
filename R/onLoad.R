default_options <- list(
  summarizeNHTS.format.digits = 2,
  summarizeNHTS.format.percentage = function(tbl) attr(tbl, 'prop'),
  summarizeNHTS.format.scientific = FALSE,
  summarizeNHTS.format.multiplier = NULL,
  summarizeNHTS.output.W = 'Weighted',
  summarizeNHTS.output.U = 'Unweighted',
  summarizeNHTS.output.E = 'Std. Error',
  summarizeNHTS.output.N = 'Sample Size'
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if(any(toset)) options(default_options[toset])
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  
  if(interactive()) {
    packageStartupMessage("\nsummarizeNHTS ", paste0(packageVersion("summarizeNHTS")))
    packageStartupMessage("Sponsored by Westat (https://www.westat.com)")
    packageStartupMessage('============================================')
    packageStartupMessage("See GitHub page for more information about the package: ",
                          "https://github.com/Westat-Transportation/summarizeNHTS")
    packageStartupMessage("For more information about the National Household Travel Survey (NHTS): ",
                          "http://nhts.ornl.gov/\n")
  }
  
}


