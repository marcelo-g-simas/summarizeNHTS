#' @import ggiraph
#' @export
make_bar_chart <- function(tbl, facet = F, order = F, percentage = attr(tbl, 'prop'), interactive = T, flip_coord = F, color_palette = 'Set1', flat_print = F) {
  
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
  #tbl[, factors] <- lapply(tbl[, factors, with = F], as.factor)
  tbl[, factors] <- lapply(tbl[, factors, with = F], function(x) factor(x, levels = x))
  
  # Factor with smaller dimensions is the aes x variable,
  # Factor with larger dimensions is the "facet by" variable
  factor_dim <- sapply(tbl[, factors, with = F], function(x) length(levels(x)))
  x_var <- names(factor_dim[ order(-factor_dim)][1])
  facet_var <- names(factor_dim[ order(-factor_dim)][2])
  
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
  
  # Create tooltip
  if(max(na.omit(tbl[['W']])) >= 100000) {
    if(prop == F) {
      tbl$tooltip <- paste0(
        '<b>', tbl[[config_tooltip_title]], '</b><br>',
        formatC(tbl[['W']], format="E", 2),' &plusmn; ', formatC(tbl[['E']], format="E", 2)
      )
    } else {
      tbl$tooltip <- paste0('<b>', tbl[[config_tooltip_title]], '</b><br>',formatC(tbl[['W']], format="E", 2))
    }

  } else {
    if(prop == F) {
      tbl$tooltip <- paste0(
        '<b>', tbl[[config_tooltip_title]], '</b><br>',
        round(tbl[['W']],3),' &plusmn; ', round(tbl[['E']],3)
      )
    } else {
      tbl$tooltip <- paste0('<b>', tbl[[config_tooltip_title]], '</b><br>',round(tbl[['W']],3))
    }

  }
  
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
  if(prop == F) {
    g <- g + geom_errorbar(
      aes(ymax = CI_max, ymin = CI_min),
      position = config_position,
      colour = '#d8490b',
      width = 0.25,
      alpha = 0.7
    )
  }
  
  # If a multiple factors, add a facet grid
  if(!is.na(facet_var) & facet == TRUE) g <- g + facet_grid(reformulate('.', facet_var), scales = 'free_y')
  
  # Specify theme
  g <- g + config_scale
  g <- g + labs(x = factors_label[[x_var]], y = agg_label)
  g <- g + theme_minimal()
  #g <- g + ggtitle(paste0(response_label,'by\n',paste0(unlist(factors_label),collapse = ' &\n')))
  g <- g + theme(plot.title = element_text(hjust = 0.5))
  g <- g + theme(strip.text.y = element_text(angle = 0))
  g <- g + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  g <- g + config_legend
  if(flip_coord) g <- g + coord_flip()
  if(percentage) g <- g + scale_y_continuous(labels=function(x) {paste0(100 * x,'%')})
  
  if(flat_print == F) {
    ggiraph(code = print(g), hover_css = "opacity: 0.5;stroke: #ffec8b; cursor: crosshair;")
  } else g
  
}
