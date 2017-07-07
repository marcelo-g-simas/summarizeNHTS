#' @export
#get_wgt_names
get_wgt_names <- function(wgt_name) {

  if(!wgt_name %in% c('HHWGT','WTPERFIN','DAYWGT','SFWGT')) {
    stop(sprintf('%s is not a valid weight column name/prefix. Use "HHWGT", "WTPERFIN", "DAYWGT", or "SFWGT"',wgt_name))
  }

  return(c(wgt_name, paste0(wgt_name, 1:100)))

}
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

  varlabs <- labels[ variable %in% vars & !grepl('[0-9 ,]+-[0-9 ,]+',code), ]
  varlabs <- varlabs[!(code == '' | label == ''), label := gsub("'","",label)]
  s <- split(varlabs, varlabs$variable)

  message('Overwriting values with labels in table ', dQuote(deparse(substitute(dt))) ,' for variable: ')
  for(i in names(s)) {
    message(dQuote(i))
    v <- s[[i]]
    var_class <- class(dt[[i]])
    class(v$code) <- var_class

    merged <- merge(dt, v, by.x = i, by.y = 'code', all.x = T, sort = F)

    dt[[i]] <- merged[,ifelse(variable != i | is.na(variable), get(i), label)]

  }

  return(dt)

}
