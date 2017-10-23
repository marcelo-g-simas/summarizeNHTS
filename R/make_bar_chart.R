#' @title Make Interactive Bar Charts
#' 
#' @description Make interactive bar charts utilizing \link[ggiraph]{ggiraph}.
#' 
#' @param tbl data.table object returned by \link[summarizeNHTS]{make_table}
#' @param facet logical. Whether to call \link[ggplot]{facet_grid} when using more than one variable
#' @param order logical. Order by values instead of natural codebook order?
#' @param color_pallete A color pallete. See \link[RColorBrewer]{display.brewer.all}
#' @param interactive logical. Print ggiraph object without interactivity?
#' @param flat_print logical. Print as ggpplot object instead ggiraph object?
#' @param squeeze_coord character. "x" or "y". The default ("x") means the variable with the most groups will be the bar chart's x variable
#' @param flip_coord logical. Flip x and y axes?
#' @param digits integer. Number of significant digits to use.
#' @param percentage logical. Treat proportions as percentages?
#' @param scientific logical. Use scientific notation for number formatting?
#' @param multiplier numeric. A multiplier to use for numeric value display (i.e. For "In Thousands", use multiplier = 1000)
#' 
#' @import ggiraph
#' @export
make_bar_chart <- function(tbl, facet = F, order = F, color_palette = 'Set1', 
                           interactive = T, flat_print = F, squeeze_coord = "x", flip_coord = F,
                           digits = 2, percentage = attr(tbl, 'prop'), scientific = F, multiplier = NULL) {
  
  factors <- attr(tbl,'factors')
  agg_label <- attr(tbl,'agg_label')
  error <- attr(tbl,'error')
  factors_label <- attr(tbl,'factors_label')
  prop <- attr(tbl,'prop')
  
  if(length(factors) > 2) {
    warning('Failed to construct chart: Table contains more than 2 factors.')
    return()
  }
  
  # Coerce all character variables as factors
  tbl[, factors] <- lapply(tbl[, factors, with = F], function(x) factor(x, levels = unique(x)))
  
  # by default, squeeze_coord=x, meaning:
  # Factor with smaller dimensions is the aes x variable,
  # Factor with larger dimensions is the "facet by" variable
  factor_dim <- sapply(tbl[, factors, with = F], function(x) length(levels(x)))
  factor_names <- c(names(factor_dim[order(-factor_dim)][1]),names(factor_dim[order(-factor_dim)][2]))
  if (tolower(squeeze_coord)=="x") {
  	x_var <- factor_names[1]
  	facet_var <- factor_names[2]
  } else {
  	x_var <- factor_names[2]
  	facet_var <- factor_names[1]
  }
  
  # Reorder x_var factor levels by agg_label variable
  if(order == T) tbl[[x_var]] <- tbl[, reorder(get(x_var), W)]
  
  # Create confidence interval variables
  tbl$CI_max <- tbl[['W']] + tbl[['E']]
  tbl$CI_min <- tbl[['W']] - tbl[['E']]
  tbl$CI_min <- ifelse(tbl$CI_min < 0, 0, tbl$CI_min)
  
  # Configure position parameters
  if(facet == T | is.na(facet_var)) {
    config_position <- 'dodge'
    config_scale <- scale_fill_continuous(low="#daadec", high="#5f416b")
    config_legend <- theme(legend.position = 'none')
    config_fill <- 'W'
    config_tooltip_title <- x_var
  } else {
    config_position <- position_dodge(width = 0.9)
    config_scale <- scale_fill_brewer(palette= color_palette)
    config_legend <- theme(legend.position = 'right')
    config_fill <- facet_var
    config_tooltip_title <- facet_var
  }
  
  # Create formatted copy of table for the tooltip
  formatted_tbl <- copy(tbl[, .(W, E)])
  formatted_tbl <- lapply(
    X = formatted_tbl, 
    FUN = format_values, 
    digits = digits,
    percentage = percentage,
    scientific = scientific,
    multiplier = multiplier
  )
  
  # Create tooltip
  tbl$tooltip <- sprintf('<b>%s</b><br>%s &plusmn; %s', 
    tbl[[config_tooltip_title]], 
    formatted_tbl[['W']], 
    formatted_tbl[['E']]
  )
  
  # Initiate ggplot object
  g <- ggplot(tbl, aes_string(x_var, 'W', fill = config_fill, group = facet_var))
  
  # Add ggiraph bar chart interactivity
  if(interactive) {
    g <- g + geom_bar_interactive(
      aes(tooltip = tooltip, data_id = tooltip),
      stat = "identity", 
      position = config_position
    )
  } else {
    g <- g + geom_bar(stat = "identity", position = config_position)
  }
  
  # Add error bars
  g <- g + geom_errorbar(
    aes(ymax = CI_max, ymin = CI_min),
    position = config_position,
    colour = '#d8490b',
    width = 0.25,
    alpha = 0.7
  )
  
  # If a multiple factors, add a facet grid
  if(!is.na(facet_var) & facet == TRUE) g <- g + facet_grid(reformulate('.', facet_var), scales = 'free_y')
  
  # Specify theme
  g <- g + config_scale
  g <- g + labs(x = factors_label[[x_var]], y = agg_label)
  g <- g + theme_minimal()
  g <- g + theme(plot.title = element_text(hjust = 0.5))
  g <- g + theme(strip.text.y = element_text(angle = 0))
  g <- g + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  g <- g + config_legend
  if(flip_coord) g <- g + coord_flip()
  g <- g + scale_y_continuous(labels = function(x) {
    format_values(x, 
      digits = ifelse(percentage, 0, digits), 
      percentage = percentage, 
      scientific = scientific, 
      multiplier = multiplier
    )
  })

  if(flat_print == F) {
    ggiraph(code = print(g), hover_css = "opacity: 0.5;stroke: #ffec8b; cursor: crosshair;")
  } else g
  
}
