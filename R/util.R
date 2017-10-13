####################################################################################################
###################################### Configuration Functions #####################################
####################################################################################################
#' @export
# ID
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
# WGT
WGT <- function(level) {
  wgt <- switch(
    EXPR = level,
    household = 'HHWGT',
    person = 'WTPERFIN',
    trip = 'DAYWGT'
  )
  if(is.null(wgt)) stop(level,' is not a valid weight level.')
  replicates <- paste0(wgt, 1:100)
  return(c(wgt, replicates))
}

####################################################################################################
####################################################################################################
####################################################################################################

#==================================================================================================#
#' @export
#use_labels
use_labels <- function(dt, keep = NULL, drop = NULL) {

  labels <- get(paste0('nhts_',attr(dt, 'dataset')))[['labels']]
  
  if(!is.null(keep)) {

    vars <- colnames(dt)[colnames(dt) %in% keep]
    if(!is.null(drop)) warning('Ignoring "drop" paramater, because "keep" was specified.')

  } else if(!is.null(drop)) {

    vars <- colnames(dt)[!colnames(dt) %in% drop]

  } else vars <- colnames(dt)

  varlabs <- labels[ NAME %in% vars & !grepl('[0-9 ,]+-[0-9 ,]+',VALUE), ]
  varlabs <- varlabs[!(VALUE == '' | DESCRIPTION == ''), DESCRIPTION := gsub("'","",DESCRIPTION)]
  s <- split(varlabs, varlabs$NAME)

  #message('Overwriting values with labels in table ', dQuote(deparse(substitute(dt))) ,' for variable: ')
  for(i in names(s)) {
    v <- s[[i]]
    var_class <- class(dt[[i]])
    class(v$VALUE) <- var_class

    merged <- merge(dt, v, by.x = i, by.y = 'VALUE', all.x = T, sort = F)
    
    dt[[i]] <- merged[,ifelse(NAME != i | is.na(NAME), get(i), DESCRIPTION)]
    dt[, (i) := factor(get(i), levels = unique(c(v$DESCRIPTION, dt[[i]])))]
  }

  return(dt)

}
#==================================================================================================#
# Vectorizing formatting function for standard formatting across multiple functions
#' @export
#format_values
format_values <- function(x, digits = 2, percentage = FALSE, scientific = FALSE, multiplier = NULL) {
  format_flag <- ifelse(scientific == F, 'f', 'E')
  if (!is.null(multiplier)) x <- x / multiplier
  if (percentage == T) {
    x <- paste0(formatC(100 * x, format = format_flag, digits = digits), '%')
  }
  x <- formatC(x, format=format_flag, digits = digits, big.mark=",")
  x <- trimws(x)
  return(x)
}

#==================================================================================================#
#' @export
#crosstab_output
crosstab_output <- function(W = 'Weighted', E = 'Std. Error', S = 'Surveyed') {
  c(W = W, E = E, S = S)
}

#==================================================================================================#
#' @export
#get_trip_weights
get_trip_weights <- function(data) {
  person_weights <- copy(data$weights$person)
  person_weight_names <- WGT('person')
  trip_weight_names <- WGT('trip')
  person_weights[, (person_weight_names) := lapply(.SD, function(x) x * 365), .SDcols = person_weight_names]
  setnames(person_weights, person_weight_names, trip_weight_names)
  setkeyv(person_weights, c(ID('household'), ID('person')))
  trip_weights <- merge(copy(data$weights$trip_keys), person_weights)
  setkeyv(trip_weights, c(ID('household'), ID('person'), ID('trip')))
  return(trip_weights)
}

#==================================================================================================#
#' @export
#select_all
select_all <- function(dataset) {
  codebook <- get(paste0('nhts_',dataset))
  all_variables <- codebook$variables$DELIVERY_NAME
  ids <- sapply(c('household','person','vehicle','trip'), ID)
  wgts <- sapply(c('household','person','trip'), function(x) WGT(x)[1])
  other_exclusions <- c('WTHHFIN','WTPERFIN','WTTRDFIN','TDCASEID')
  exclude <- c(ids, wgts, other_exclusions)
  return(all_variables[!all_variables %in% exclude])
}



