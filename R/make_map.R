#' Create map from spatially aggregated NHTS data
#'
#' @param table1 table returned from \link[summarizeNHTS]{make_table}
#' @param geography "state", "cbsa"
#' @param table2 same as table1, but optional second table to show inside tooltip when geography is hovered with mouse
#' @return ggiraph/htmlwidget class object
#' @examples
#' @export

make_map <- function(table1, geography = "state", table2) {
  library(ggiraph)
  if(missing(table1)) {
    stop("table1 parameter is not set")
  }
  if(!("dataset" %in% names(attributes(table1)))) {
    stop("table1 does not appear to be a table returned by make_table()")
  }
  
  # state_layer
  # cbsa_layer
  
  if(tolower(geography) == "state") {
    geog_var <- "HHSTATE"
    geog_id <- "STATEFP"
  }
  if(tolower(geography) == "cbsa") {
    geog_var <- "HH_CBSA"
    geog_id <- "CSAFP"
  }
  
  agg_name <- colnames(table1)[colnames(table1) %in% c("household_trip_rate", "person_trip_rate", "household_count", "person_count", "trip_count", "sum", "avg")]
  
  # merge table1 with state_layer 
  merged <- merge(x = state_layer, y = table1, by.x = 'STUSPS', by.y = 'HHSTATE')
  
  if(!missing(table2)) {
      
    if(!("dataset" %in% names(attributes(table2)))) {
      stop("table2 does not appear to be a table returned by make_table()")
    }
  
    table2_factors <- attributes(table2)$factors[attributes(table2)$factors != geog_var]
    table2 <- use_labels(table2, keep = c(table2_factors))
  
    # list of attributes to copy for each data.table split/subset
    attr_names <- names(attributes(table2))[!names(attributes(table2)) %in% c("class","row.names",".internal.selfref")]
    
    # split the data.table by HHSTATE and create svg for plot of each state
    progress_bar <- txtProgressBar(min = 0, max = length(with(table2, split(table2, HHSTATE))), style = 3)
    gg_html <- sapply(with(table2, split(table2, HHSTATE)), function(tbl) {
      
      current_index <- which(names(with(table2, split(table2, HHSTATE)))==names(with(table2, split(table2, HHSTATE)))[names(with(table2, split(table2, HHSTATE)))==unlist(tbl[1, "HHSTATE"])])
      setTxtProgressBar(progress_bar, current_index)
      
      for(i in attr_names) {
        setattr(tbl, i, attr(table2, i))
      }
      g <- make_bar_chart(tbl, interactive = F, order = F)
      g <- gsub("'", "\"", g$x$html) #remove single quotes from html string
      return(g)
      #htmltools::htmlEscape(g$x$html, FALSE)

    })
    close(progress_bar) # close progress bar
    
    #Create column of state names for merging
    gg_html <- data.frame(STUSPS = attr(gg_html, "names"), tooltip = unlist(gg_html), row.names=NULL, stringsAsFactors=F)
  
    # merge the table2 tooltip with state_layer/table1 merge
    merged <- merge(merged, gg_html, by = 'STUSPS')

  }
  
  if(is.null(merged$tooltip)) {
    merged$tooltip <- ""
  }
  
  geom_state_layer <- geom_polygon_interactive(
    data = merged,
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      tooltip = paste0(NAME, "\n", tooltip),
      fill = get(agg_name),
      data_id = geog_id),
    color = "#FFFFFF",
    size = 0.35)
  
  #Create map
  map <- ggplot() + 
    geom_state_layer + 
    coord_fixed() + 
    theme_void() + 
    scale_fill_gradient(low = "#f2f0f7", high = "#54278f")
  
  tooltip_css <- "background-color:#F2F2F2;padding:10px;border-radius:10px 20px 10px 20px;"
  
  map_widget <- ggiraph(code = {print(map)}, tooltip_extra_css = tooltip_css, width_svg = 8, height_svg = 8)
  map_widget
  
}