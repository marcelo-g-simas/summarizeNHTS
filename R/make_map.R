#' @import data.table
NULL

#' Create a map from geographically aggregated NHTS data
#'
#' @param table1 table returned from \link[summarizeNHTS]{make_table}
#' @param geography "state", "cbsa"
#' @param table2 same as table1, but optional second table to show inside tooltip when geography is hovered with mouse
#' @return ggiraph/htmlwidget class object
#' @examples
#' @export
make_map <- function(table1, geography = "state", table2) {

  if(missing(table1)) {
    stop("table1 parameter is not set")
  }
  if(!("dataset" %in% names(attributes(table1)))) {
    stop("table1 does not appear to be a table returned by make_table()")
  }
  
  if(tolower(geography) == "state") {
    geog_var <- "HHSTATE"
    merged <- merge(x = state_layer, y = table1, by.x = "STUSPS", by.y = geog_var, all.x = TRUE)
  }
  if(tolower(geography) == "cbsa") {
    geog_var <- "HH_CBSA"
    cbsa_layer$idx <- row.names(cbsa_layer) # stash index order because merge does not preverse order, important when mapped by geom_etc.
    merged <- merge(x = cbsa_layer, y = table1, by.x = "CBSAFP", by.y = geog_var, all.x = TRUE)
    merged <- merged[order(merged$idx), ]
    merged$idx <- NULL
  }
  colnames(merged)[1] <- geog_var
  
  agg_name <- colnames(table1)[colnames(table1) %in% c("household_trip_rate", "person_trip_rate", "household_count", "person_count", "trip_count", "sum", "avg")]
  
  if(!missing(table2)) {
      
    if(!("dataset" %in% names(attributes(table2)))) {
      stop("table2 does not appear to be a table returned by make_table()")
    }
  
    table2_factors <- attributes(table2)$factors[attributes(table2)$factors != geog_var]
    table2 <- use_labels(table2, keep = c(table2_factors))
  
    # list of attributes to copy for each data.table split/subset
    attr_names <- names(attributes(table2))[!names(attributes(table2)) %in% c("class","row.names",".internal.selfref")]
    
    # split data.table by geography variable and create svg for each
    progress_bar <- txtProgressBar(min = 0, max = length(with(table2, split(table2, get(geog_var)))), style = 3)
    gg_html <- sapply(with(table2, split(table2, get(geog_var))), function(tbl) {
      
      current_index <- which(names(with(table2, split(table2, get(geog_var))))==names(with(table2, split(table2, get(geog_var))))[names(with(table2, split(table2, get(geog_var))))==unlist(tbl[1, ..geog_var])])
      setTxtProgressBar(progress_bar, current_index)
      
      for(i in attr_names) {
        setattr(tbl, i, attr(table2, i))
      }
      g <- make_bar_chart(tbl, interactive = F)
      g <- gsub("'", "\"", g$x$html) # single quotes not supported in tooltips
      g <- gsub("[\n]", " ", g) # hard \n also not supported https://github.com/davidgohel/ggiraph/issues/18
      return(g)

    })
    close(progress_bar) # close progress bar
    
    # wrap up list object of bar charts in data.frame for merging back to data
    gg_html <- data.frame(foo = attr(gg_html, "names"), tooltip = unlist(gg_html), row.names=NULL, stringsAsFactors=F)
    names(gg_html)[1] <- geog_var
    merged <- merge(merged, gg_html, by = geog_var)

  }
  
  if(is.null(merged$tooltip)) {
    merged$tooltip <- ""
  }
  merged$NAME <- gsub("'", "&apos;", merged$NAME)
  
  geog_layer <- geom_polygon_interactive(
    data = merged,
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = get(agg_name),
      tooltip = paste0(NAME, "<br>", tooltip),
      data_id = group
      ),
    color = "#FFFFFF",
    size = 0.35)
  
  # if not mapping state level data, at least provide state border as frame of reference
  if(geography!="state") {
    state_border <- geom_polygon_interactive(data=state_layer, mapping = aes(x = long, y = lat, group = group), color = "#999999", size = 0.35, fill = NA)
  } else {
    state_border <- NULL
  }
  
  map <- ggplot() +
    state_border +
    geog_layer +
    coord_fixed() + 
    theme_void() + 
    scale_fill_gradient(low = "#f2f0f7", high = "#54278f")
  
  tooltip_css <- "background-color:#F2F2F2; padding:10px; border-radius:10px 20px 10px 20px"
  tooltip_css <- ifelse(!missing(table2), paste0(tooltip_css, "; width:400px"), tooltip_css) # give sensible fixed width to tooltips with charts
  
  map_widget <- ggiraph(code = {print(map)}, tooltip_extra_css = tooltip_css, hover_css = "border:0; fill:#fff7bc", width_svg = 8, height_svg = 8)
  map_widget
  
}