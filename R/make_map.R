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
make_map <- function(tbl, table2, state_style = "normal", ...) {

  dataset <- attr(tbl, 'dataset')
  values <- CB(dataset)$values
  group_var <- attr(tbl, 'by')
  
  format_map_values <- function(x) {
    do.call(format_values, c(list(x = x), list(...)))
  }

  if(length(group_var) > 1) {
    stop('tbl parameter has too many "by" variables.',
         '\ntbl can only be a single geography group variable (one value per geography record)',
         '\nConsider using table2 parameter feature if you want to further group geography level data')
  }
  
  choose_state_layer <- function(x) {
    if (x == 'normal') {
      state_layer
    } else if (x == 'tile') {
      state_tile_layer
    } else {
      stop('state_style parameter must be either "normal" or "tile"')
    }
  }
  
  geo_layer <- switch(group_var,
    'HHSTFIPS' = choose_state_layer(state_style),
    'HH_CBSA' = cbsa_layer,
    'CENSUS_R' = census_region_layer,
    'CENSUS_D' = census_division_layer
  )
  
  if (is.null(geo_layer)) {
    stop(sprintf('Table has incompatible "by" variable: %s', group_var))
  }
  
  if (attr(tbl,'label') == TRUE) {
    # If value labels were used, lookup value in codebook
    labeled_tbl <- merge(
      x = tbl, 
      y = values[NAME == group_var, list(VALUE, LABEL)], 
      by.x = group_var, 
      by.y = 'LABEL', 
      all.x = TRUE
    )
    by_y <- 'VALUE'
  } else {
    labeled_tbl <- tbl
    by_y <- group_var
  }
  
  # Merge table with geography layer
  merged <- merge(
    x = geo_layer, 
    y = labeled_tbl, 
    by.x = 'GEOID', 
    by.y = by_y, 
    all.x = TRUE
  )
 
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
    formatted_tbl <- lapply(X = formatted_tbl, FUN = format_map_values)
    
    # Create tooltip
    merged$tooltip <- sprintf('%s &plusmn; %s', 
      formatted_tbl[['W']], 
      formatted_tbl[['E']]
    )
    
  }
  
  merged$NAME <- gsub("'", "&apos;", merged$NAME)
  
  gglayer <- geom_polygon_interactive(
    data = merged,
    color = "#FFFFFF",
    size = 0.35,
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = W,
      tooltip = paste0("<b>", NAME, "</b>", "<br>", tooltip),
      data_id = group
    )
  )
  
  # if not mapping state level data, at least provide state border as frame of reference
  if (attr(geo_layer, 'layer_name') == 'cbsa_layer') {
    state_border <- geom_polygon_interactive(
      data = state_layer, 
      mapping = aes(
        x = long,
        y = lat,
        group = group
      ), 
      color = "#999999", 
      size = 0.35, 
      fill = NA
    )
  } else {
    state_border <- NULL
  }
  
  # put a geom_text label in the center of the polygon
  if (attr(geo_layer, 'layer_name') == 'state_tile_layer') {
    label_centroid <- merge(state_tile_layer, values[NAME == 'HHSTATE'], by.x = 'NAME', by.y = 'LABEL')
  	label_centroid <- label_centroid[, list(long = mean(long), lat = mean(lat)), by = VALUE]
  	label_text <- geom_text_interactive(data = label_centroid, aes(label = VALUE, x = long, y = lat))
  } else {
  	label_text <- NULL
  }
  
  
  
  map <- ggplot() +
    state_border +
    gglayer +
    label_text +
    coord_fixed() + 
    theme_void() + 
    scale_fill_gradient(
      low = "#f2f0f7", 
      high = "#54278f", 
      labels = format_map_values
    )
  
  tooltip_css <- "background-color:#F2F2F2; padding:10px; border-radius:10px 20px 10px 20px"
  tooltip_css <- ifelse(!missing(table2), paste0(tooltip_css, "; width:400px"), tooltip_css) # give sensible fixed width to tooltips with charts
  
  map_widget <- ggiraph(
    code = {print(map)}, 
    tooltip_extra_css = tooltip_css, 
    hover_css = "border:0; fill:#fff7bc", 
    width_svg = 8, height_svg = 8
  )
  
  return(map_widget)
  
}