#' @import data.table
NULL

#' Create a map from geographically aggregated NHTS data
#'
#' @param tbl table returned from \link[summarizeNHTS]{make_table}, requires at least one group variable in tbl to be either HHSTATE or HH_CBSA (2009, 2017 only)
#' @param tbl2 optional second table returned from \link[summarizeNHTS]{make_table}, requires same geography group variable as tbl. tbl2 gets passed to \link[summarizeNHTS]{make_bar_chart} and output as an interactive tooltip over the matching tbl geography
#' @param state_style either "normal" for typical state map boundaries or "tile" for tilemap style fixed-area boundaries
#' @param digits integer. Number of significant digits to use.
#' @param percentage logical. Treat proportions as percentages?
#' @param scientific logical. Use scientific notation for number formatting?
#' @param multiplier numeric. A multiplier to use for numeric value display (i.e. For "In Thousands", use multiplier = 1000)
#' @return ggiraph/htmlwidget class object
#' @examples
#' @export
make_map <- function(tbl, tbl2, state_style = "normal", ...) {

  if (!'HTS.summary.table' %in% class(tbl)) {
    stop('tbl argument is not an "HTS.summary.table" object (returned by the summarize_data function).')
  }
  
  dataset <- attr(tbl, 'dataset')
  values <- CB(dataset)$values
  group_var <- attr(tbl, 'by')
  prop <- attr(tbl, 'prop')
  
  # If percentage argument is not passed, set the default
  format_arguments <- list(...)
  if(!'percentage' %in% names(format_arguments)) {
    format_arguments <- c(format_arguments, percentage = prop)
  }
  
  format_map_values <- function(x) {
    do.call(format_values, c(list(x = x), format_arguments))
  }

  if(length(group_var) > 1) {
    stop('tbl parameter has too many "by" variables.',
         '\ntbl can only be a single geography group variable (one value per geography record)',
         '\nConsider using tbl2 parameter feature if you want to further group geography level data')
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
 
  if(!missing(tbl2)) {
    
    if (!'HTS.summary.table' %in% class(tbl)) {
      stop('tbl2 argument is not an "HTS.summary.table" object (returned by the summarize_data function).')
    }
    
    progress_bar <- txtProgressBar(min = 0, max = nrow(tbl), style = 3)
    current_index <- 0
    
    tbl2_group_var <- attr(tbl2, 'by')[!attr(tbl2, 'by') %in% group_var]
    
    gg_html <- sapply(split(tbl2, by = group_var), function(tbl) {
      current_index <<- current_index + 1
      setTxtProgressBar(progress_bar, current_index)
      setattr(tbl, 'by', tbl2_group_var)
      g <- make_chart(tbl, interactive = F)
      g <- gsub("'", "\"", g$x$html) # single quotes not supported in tooltips
      g <- gsub("[\n]", " ", g) # hard \n also not supported https://github.com/davidgohel/ggiraph/issues/18
      return(g)
    })
    close(progress_bar)
    
    # wrap up list object of bar charts in data.frame for merging back to data
    gg_html <- data.table(
      geography = attr(gg_html, "names"),
      tooltip = unlist(gg_html)
    )
    merged <- merge(merged, gg_html, by.x = group_var, by.y = 'geography')
    
  }
  
  # create tooltip when interactive tbl2 tooltip method is not used
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
  tooltip_css <- ifelse(!missing(tbl2), paste0(tooltip_css, "; width:400px"), tooltip_css) # give sensible fixed width to tooltips with charts
  
  map_widget <- ggiraph(
    code = {print(map)}, 
    tooltip_extra_css = tooltip_css, 
    hover_css = "border:0; fill:#fff7bc", 
    width_svg = 8, height_svg = 8
  )
  
  return(map_widget)
  
}