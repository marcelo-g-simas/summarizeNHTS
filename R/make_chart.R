#' @title Make Interactive Charts
#' 
#' @description Make interactive bar charts utilizing \link[ggiraph]{ggiraph}.
#' 
#' @param tbl data.table object returned by \link[summarizeNHTS]{make_table}.
#' @param x character. Variable to be displayed along x-axis.
#' @param y character. Either 'W' for weighted or 'U' for unweighted statistic.
#' @param fill character. Variable to be categorized using interior coloring.
#' @param facet logical. Variable to be categorized by faceted grid.
#' @param interactive logical. Print as ggpplot object instead ggiraph object?
#' @param order logical. Order by values instead of natural codebook order?
#' @param flip logical. Flip x and y axes?
#' @param flat_print logical. Print as ggpplot object instead ggiraph object?
#' @param palette character. A color pallete to be used for the fill variable. See \link[RColorBrewer]{display.brewer.all}
#' @param digits integer. Number of significant digits to use.
#' @param percentage logical. Treat proportions as percentages?
#' @param scientific logical. Use scientific notation for number formatting?
#' @param multiplier numeric. A multiplier to use for numeric value display (i.e. For "In Thousands", use multiplier = 1000)
#' 
#' @import ggiraph
#' @export
make_chart <- function(tbl, x = NULL, y = NULL, fill = NULL, facet = NULL, interactive = TRUE,
                       order = FALSE, flip = FALSE, flat_print = FALSE, palette = 'Set1',
                       digits = 2, percentage = attr(tbl, 'prop'), scientific = F, multiplier = NULL,...) {
  
  
  if(is.null(y)) y <- 'W'
  
  by <- attr(tbl,'by')
  agg_label <- attr(tbl,'agg_label')
  error <- attr(tbl,'error')
  by_label <- attr(tbl,'by_label')
  prop <- attr(tbl,'prop')
  prop_by <- NULL
  
  # Coerce all character variables as factors
  tbl[, by] <- lapply(tbl[, ..by], function(x) factor(x, levels = unique(x)))
  
  group_count <- length(by)
  group_level_count <- sapply(tbl[, ..by], function(x) length(levels(x)))
  groups_sorted <- names(sort(group_level_count))
  
  choose_group <- function(f) {
    if(is.null(f)) f <- groups_sorted[!groups_sorted %in% c(x, facet, fill)][1]
    return(f)
  }
  
  if (group_count == 1) {
    
    x <- choose_group(x)
    if (!is.null(fill)) warning('fill parameter not used with 1 group variable.')
    if (!is.null(facet)) warning('facet parameter not used with 1 group variable.')
    fill <- NULL
    facet <- NULL
    
  } else if (group_count == 2) {
    
    if(!is.null(facet) & !is.null(fill)) {
      warning('Cannot specify fill and facet with 2 group variables. Only fill parameter will be used.')
      facet <- NULL
    } 
    x <- choose_group(x)
    if (is.null(fill)) fill <- prop_by
    if (is.null(facet)) fill <- choose_group(fill)
    
  } else if (group_count == 3) {
    
    if (is.null(facet)) {
      facet <- prop_by
      facet <- choose_group(facet)
    }
    x <- choose_group(x)
    fill <- choose_group(fill)
    
  } else stop('Cannot construct a chart with more than 3 group variables.')
  
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
  tbl$CI_min <- ifelse(tbl$CI_min < 0, 0, tbl$CI_min)
  
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
    paste0(tbl[[x]], sprintf('<br>%s', tbl[,..by][[fill]])),
    formatted_tbl[['W']], 
    formatted_tbl[['E']]
  )
  
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
  g <- g + labs(x = by_label[[x]], y = agg_label)
  g <- g + theme_minimal()
  g <- g + theme(plot.title = element_text(hjust = 0.5))
  g <- g + theme(strip.text.y = element_text(angle = 0))
  g <- g + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  g <- g + theme(legend.position = 'right')
  if(flip) g <- g + coord_flip()
  g <- g + scale_y_continuous(labels = function(x) {
    format_values(x, 
      digits = ifelse(percentage, 0, digits), 
      percentage = percentage, 
      scientific = scientific, 
      multiplier = multiplier
    )
  })
  
  if(flat_print == F) {
    ggiraph(code = print(g), hover_css = "opacity: 0.5;stroke: #ffec8b; cursor: crosshair;", ...)
  } else g
  
}


