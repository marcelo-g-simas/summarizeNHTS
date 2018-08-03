#' @title Make Interactive Charts
#' 
#' @description Make interactive bar charts utilizing \link[ggiraph]{ggiraph}.
#' 
#' @param tbl data.table object returned by \link[summarizeNHTS]{summarize_data}.
#' @param x character. Variable to be displayed along x-axis.
#' @param y character. Either 'W' for weighted or 'U' for unweighted statistic.
#' @param fill character. Variable to be categorized using interior coloring.
#' @param facet logical. Variable to be categorized by faceted grid.
#' @param interactive logical. Print as ggpplot object instead ggiraph object?
#' @param order logical. Order by values instead of natural codebook order?
#' @param flip logical. Flip x and y axes?
#' @param flat_print logical. Print as ggpplot object instead ggiraph object?
#' @param legend logical. Display a legend?
#' @param variable_labels logical. Use variable labels/descriptions?
#' @param palette character. A color pallete to be used for the fill variable. See pallete argument here \link[ggplot2:scale_brewer]{scale_fill_brewer}.
#' @param confidence Confidence level for margin of error calculation. Defaults to 0.95. Set to NULL for standard error.
#' @param label_options List of label options when variable_labels = TRUE. Number of characters before wrapping and truncating text, respectively.
#' @param ggiraph_options Optional list of options to pass to \link[ggiraph]{ggiraph}.
#' @param ... Optional formatting arguments. See \link[summarizeNHTS]{format_values}.
#' 
#' @import ggiraph
#' 
#' @export
make_chart <- function(tbl, x = NULL, y = NULL, fill = NULL, facet = NULL, interactive = TRUE, order = FALSE, 
                       flip = FALSE, flat_print = FALSE, legend = TRUE, variable_labels = TRUE, palette = 'Set1',
                       confidence = 0.95, label_options = list(wrap = 35, trunc = 100), ggiraph_options = list(), ...) {
  
  if (!'HTS.summary.table' %in% class(tbl)) {
    stop('tbl argument is not an "HTS.summary.table" object (returned by the summarize_data function).')
  }
  
  # Apply margin of error
  tbl <- use_moe(tbl, confidence)
  
  if(is.null(y)) y <- 'W'
  
  by <- attr(tbl,'by')
  by_label <- attr(tbl,'by_label')
  agg_label <- attr(tbl,'agg_label')
  agg_var <- attr(tbl, 'agg_var')
  agg_var_label <- attr(tbl,'agg_var_label')
  error <- attr(tbl,'error')
  prop <- attr(tbl,'prop')
  prop_by <- NULL
  
  # If percentage argument is not passed, set the default
  format_arguments <- list(...)
  if(!'percentage' %in% names(format_arguments)) {
    format_arguments <- c(format_arguments, percentage = prop)
  }
  
  format_chart_values <- function(x) {
    do.call(format_values, c(list(x = x), format_arguments))
  }
  
  # Get count of group variables
  group_count <- length(by)
  
  # Global group variable configurations
  if (group_count > 0) {
    # Coerce all character variables as factors
    tbl[, by] <- lapply(tbl[, ..by], function(x) factor(x, levels = unique(x)))
    group_level_count <- sapply(tbl[, ..by], function(x) length(levels(x)))
    groups_sorted <- names(sort(group_level_count))
  }
  
  # Function to dynamically choose which variables to select
  choose_group <- function(f) {
    if(is.null(f)) f <- groups_sorted[!groups_sorted %in% c(x, facet, fill)][1]
    return(f)
  }
  
  # No Group Variables
  if (group_count == 0) {
    
    x <- factor('')
    fill <- NULL
    facet <- NULL
  
  # 1 Group Variable
  } else if (group_count == 1) {
    
    x <- choose_group(x)
    if (!is.null(fill)) warning('fill parameter not used with 1 group variable.')
    if (!is.null(facet)) warning('facet parameter not used with 1 group variable.')
    fill <- NULL
    facet <- NULL
  
  # 2 Group Variables
  } else if (group_count == 2) {
    
    if(!is.null(facet) & !is.null(fill)) {
      warning('Cannot specify fill and facet with 2 group variables. Only fill parameter will be used.')
      facet <- NULL
    } 
    x <- choose_group(x)
    if (is.null(fill)) fill <- prop_by
    if (is.null(facet)) fill <- choose_group(fill)
    
  # 3 Group Variables  
  } else if (group_count == 3) {
    
    if (is.null(facet)) {
      facet <- prop_by
      facet <- choose_group(facet)
    }
    x <- choose_group(x)
    fill <- choose_group(fill)
    
  } else stop('Cannot construct a chart with more than 3 group variables.')
  
  # Use variable labels or names
  if (variable_labels == TRUE) {
    
    x_label <- unlist(by_label[x])
    x_label <- trim_label(x_label, label_options$wrap, label_options$trunc)
    
    y_label <- ifelse(length(agg_var) == 0, agg_label, sprintf('(%s) %s', agg_label, agg_var_label))
    y_label <- trim_label(y_label, label_options$wrap, label_options$trunc)
    
    fill_label <- unlist(by_label[fill])
    fill_label <- trim_label(fill_label, label_options$wrap, label_options$trunc)
    
  } else {
    x_label <- x
    y_label <- paste(agg_label, agg_var)
    fill_label <- fill
  }
  
  if(is.null(fill)) {
    fill <- y
    group <- NULL
    config_scale <- scale_fill_continuous(low="#daadec", high="#5f416b")
  } else {
    config_scale <- scale_fill_brewer(palette= palette)
    group <- fill
  }

  # Order by value
  if(order == T) tbl[[x]] <- tbl[, reorder(get(x), W)]
  
  # Create confidence interval variables
  tbl$CI_max <- tbl[['W']] + tbl[['E']]
  tbl$CI_min <- tbl[['W']] - tbl[['E']]
  tbl$CI_min <- as.numeric(ifelse(tbl$CI_min < 0, 0, tbl$CI_min))
  
  # Create formatted copy of table for the tooltip
  formatted_tbl <- copy(tbl[, .(W, E)])
  formatted_tbl <- lapply(
    X = formatted_tbl, 
    FUN = format_chart_values
  )
  
  # Create tooltip
  if (group_count > 0) {
    tbl$tooltip <- sprintf('<b>%s</b><br>%s &plusmn; %s', 
      paste0(tbl[[x]], sprintf('<br>%s', tbl[,..by][[fill]])),
      formatted_tbl[['W']], 
      formatted_tbl[['E']]
    )
  } else {
    tbl$tooltip <- sprintf('%s &plusmn; %s', 
      formatted_tbl[['W']], 
      formatted_tbl[['E']]
    )
  }

  # Initiate ggplot object
  g <- ggplot(tbl, aes_string(x, y, fill = fill, group = group))
  
  # Add ggiraph bar chart interactivity
  if(interactive) {
    g <- g + geom_bar_interactive(
      aes(tooltip = tooltip, data_id = tooltip),
      stat = "identity", 
      position = position_dodge(width = 0.9)
    )
  } else {
    g <- g + geom_bar(stat = "identity", position = position_dodge(width = 0.9))
  }
  
  # Add error bars
  g <- g + geom_errorbar(
    aes(ymax = CI_max, ymin = CI_min),
    position = position_dodge(width = 0.9),
    colour = '#d8490b',
    width = 0.25,
    alpha = 0.7
  )

  # Add Facet grid if applicable
  if(!is.null(facet)) g <- g + facet_grid(reformulate(facet), scales = 'free_y')

  # Specify theme
  g <- g + config_scale
  g <- g + labs(x = x_label, y = y_label, fill = fill_label)
  g <- g + theme_minimal()
  g <- g + theme(plot.title = element_text(hjust = 0.5))
  g <- g + theme(strip.text.y = element_text(angle = 0))
  g <- g + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  g <- g + theme(legend.position = ifelse(legend, 'right', 'none'),  legend.text = element_text(size=9))
  if(flip) g <- g + coord_flip()
  g <- g + scale_y_continuous(labels = format_chart_values)
  
  if(flat_print == F) {
    ggiraph_options_default <- list(
      ggobj = g, 
      hover_css = "opacity: 0.5;stroke: #ffec8b; cursor: crosshair;"
    )
    do.call(ggiraph, c(
        ggiraph_options_default[!names(ggiraph_options_default) %in% names(ggiraph_options)], 
        ggiraph_options
      )
    )
  } else g
  
}


