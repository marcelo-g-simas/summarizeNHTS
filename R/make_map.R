#' @import data.table
NULL

#' Create a map from geographically aggregated NHTS data
#'
#' @param table1 table returned from \link[summarizeNHTS]{make_table}, requires at least one group variable in table1 to be either HHSTATE or HH_CBSA (2009, 2017 only)
#' @param table2 optional second table returned from \link[summarizeNHTS]{make_table}, requires same geography group variable as table1. table2 gets passed to \link[summarizeNHTS]{make_bar_chart} and output as an interactive tooltip over the matching table1 geography
#' @param state_style either "normal" for typical state map boundaries or "tile" for tilemap style fixed-area boundaries
#' @param digits integer. Number of significant digits to use.
#' @param percentage logical. Treat proportions as percentages?
#' @param scientific logical. Use scientific notation for number formatting?
#' @param multiplier numeric. A multiplier to use for numeric value display (i.e. For "In Thousands", use multiplier = 1000)
#' @return ggiraph/htmlwidget class object
#' @examples
#' @export
make_map <- function(table1, table2, state_style = "normal", digits = 2, percentage = attr(table1, "prop"), scientific = F, multiplier = NULL) {

  if(missing(table1)) {
    stop("table1 parameter is not set")
  }
  if(!("dataset" %in% names(attributes(table1)))) {
    stop("table1 does not appear to be a table returned by make_table()")
  }
  
  if(!any(attributes(table1)$by %in% c("HHSTATE","HH_CBSA"))) {
    stop("table1 must have a single geography group variable, either HHSTATE (state) or HH_CBSA (city)")
  }
  
  if(length(attributes(table1)$by) > 1) {
    stop("table1 has too many group variables. table1 can only be a single geography group variable (one value per geography record) - consider using table2 parameter feature if you want to further group geography level data")
  }
  
  if("HHSTATE" %in% attributes(table1)$by) {
    if(!state_style %in% c("normal", "tile")) {
      stop("state_style parameter must be either 'normal' or 'tile'") 
    }
    if(state_style=="normal") {
      geography <- "state"
    }
    if(state_style=="tile") {
      geography <- "state_tile"
    }
  }
  
  if("HH_CBSA" %in% attributes(table1)$by) {
    geography <- "cbsa" 
  }
  
  if(geography == "state") {
    geog_var <- "HHSTATE"
    merged <- merge(x = state_layer, y = table1, by.x = "STUSPS", by.y = geog_var, all.x = TRUE)
  }
  if(geography == "state_tile") {
    geog_var <- "HHSTATE"
    merged <- merge(x = state_tile_layer, y = table1, by.x = "STUSPS", by.y = geog_var, all.x = TRUE)
  }
  if(geography == "cbsa") {
    geog_var <- "HH_CBSA"
    cbsa_layer$idx <- row.names(cbsa_layer) # stash index order because merge does not preserve order, important when mapped by geom_etc.
    merged <- merge(x = cbsa_layer, y = table1, by.x = "CBSAFP", by.y = geog_var, all.x = TRUE)
    merged <- merged[order(merged$idx), ]
    merged$idx <- NULL
  }
  colnames(merged)[1] <- geog_var
  
  if(!missing(table2)) {
      
    if(!("dataset" %in% names(attributes(table2)))) {
      stop("table2 does not appear to be a table returned by make_table()")
    }
  
    table2_by <- attributes(table2)$by[attributes(table2)$by != geog_var]
    table2 <- use_labels(table2, keep = c(table2_by))

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
  
  # create tooltip when interactive table2 tooltip method is not used
  if(is.null(merged$tooltip)) {
    
    # Create formatted copy of table for the tooltip
    formatted_tbl <- copy(merged[, c("W", "E")])
    formatted_tbl <- lapply(
      X = formatted_tbl, 
      FUN = format_values, 
      digits = digits,
      percentage = percentage,
      scientific = scientific,
      multiplier = multiplier
    )
    
    # Create tooltip
    merged$tooltip <- sprintf('%s &plusmn; %s', 
      formatted_tbl[['W']], 
      formatted_tbl[['E']]
    )
    
  }
  
  merged$NAME <- gsub("'", "&apos;", merged$NAME)
  
  geog_layer <- geom_polygon_interactive(
    data = merged,
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = W,
      tooltip = paste0("<b>", NAME, "</b>", "<br>", tooltip),
      data_id = group
      ),
    color = "#FFFFFF",
    size = 0.35)
  
  # if not mapping state level data, at least provide state border as frame of reference
  if(geography %in% c("cbsa")) {
    state_border <- geom_polygon_interactive(data=state_layer, mapping = aes(x = long, y = lat, group = group), color = "#999999", size = 0.35, fill = NA)
  } else {
    state_border <- NULL
  }
  
  # put a geom_text label in the center of the polygon
  if(geography %in% c("state_tile")) {
  	label_centroid <- data.frame(STUSPS=sort(unique(state_tile_layer$STUSPS)), long=aggregate(long~STUSPS, state_tile_layer, mean)[, 2], lat=aggregate(lat~STUSPS, state_tile_layer, mean)[, 2])
  	label_text <- geom_text_interactive(data=label_centroid, aes(label = STUSPS, x = long, y = lat))
  } else {
  	label_text <- NULL
  }
  
  map <- ggplot() +
    state_border +
    geog_layer +
    label_text +
    coord_fixed() + 
    theme_void() + 
    scale_fill_gradient(low = "#f2f0f7", high = "#54278f", labels = function(x) { format(x, big.mark = ",", scientific = scientific, trim = TRUE) })
  
  tooltip_css <- "background-color:#F2F2F2; padding:10px; border-radius:10px 20px 10px 20px"
  tooltip_css <- ifelse(!missing(table2), paste0(tooltip_css, "; width:400px"), tooltip_css) # give sensible fixed width to tooltips with charts
  
  map_widget <- ggiraph(code = {print(map)}, tooltip_extra_css = tooltip_css, hover_css = "border:0; fill:#fff7bc", width_svg = 8, height_svg = 8)
  map_widget
  
}