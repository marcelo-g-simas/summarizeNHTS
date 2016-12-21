

get_wgt_names <- function(wgt_name) {
  
  if(!wgt_name %in% c('HHWGT','WTPERFIN','DAYWGT','SFWGT')) {
    stop(sprintf('%s is not a valid weight column name/prefix. Use "HHWGT", "WTPERFIN", "DAYWGT", or "SFWGT"',wgt_name))
  }
    
  c(wgt_name, paste0(wgt_name, 1:100))
  
}

