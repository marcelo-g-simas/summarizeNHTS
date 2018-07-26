#' @title Format numeric values
#' 
#' @description Consistent formatting options across all summarizeNHTS tables/visualizations.
#' 
#' @param x Numeric vector.
#' @param digits integer. Number of decimal places to use.
#' @param percentage logical. Treat proportions as percentages?
#' @param scientific logical. Use scientific notation for number formatting?
#' @param multiplier numeric. A multiplier to use for numeric value display (i.e. For "In Thousands", use multiplier = 1000)
#' @return Formatted character vector, the same length as x.
#' @export

format_values <- function(x, 
                          digits = getOption('HTS.format.digits'), 
                          percentage = getOption('HTS.format.percentage'), 
                          scientific = getOption('HTS.format.scientific'), 
                          multiplier = getOption('HTS.format.multiplier')) {
  
  format_flag <- ifelse(scientific == F, 'f', 'E')
  if (all(x == as.integer(x), na.rm = T)) digits <- 0
  if (!is.null(multiplier)) x <- x / multiplier
  if (percentage == T) {
    x <- paste0(formatC(100 * x, format = format_flag, digits = digits), '%')
  }
  x <- formatC(x, format=format_flag, digits = digits, big.mark=",")
  x <- trimws(x)
  return(x)
}