#' @import ggiraph
#' @export
make_bar_chart <- function(tbl, facet = F, order = T, interactive = T, flip_coord = F, color_palette = 'Set1') {
  
  factors <- attr(tbl,'factors')
  response <- attr(tbl,'response')
  variance <- attr(tbl,'variance')
  factors_label <- attr(tbl,'factors_label')
  response_label <- attr(tbl,'response_label')
  
  if(length(factors) > 2) {
    warning('Failed to construct chart: Table contains more than 2 factors.')
    return()
  }
  
  # Coerce all character variables as factors
  tbl[, factors] <- lapply(tbl[, factors, with = F], as.factor)
  
  # Factor with smaller dimensions is the aes x variable,
  # Factor with larger dimensions is the "facet by" variable
  factor_dim <- sapply(tbl[, factors, with = F], function(x) length(levels(x)))
  x_var <- names(factor_dim[ order(-factor_dim)][1])
  facet_var <- names(factor_dim[ order(-factor_dim)][2])
  
  # Reorder x_var factor levels by response variable
  if(order == T) tbl[[x_var]] <- tbl[, reorder(get(x_var), get(response))]
  
  # Create confidence interval variables
  tbl$CI_max <- tbl[[response]] + tbl[[variance]]
  tbl$CI_min <- tbl[[response]] - tbl[[variance]]
  tbl$CI_min <- ifelse(tbl$CI_min < 0, 0, tbl$CI_min)
  
  # Configure position parameters
  if(facet == T) {
    config_position <- 'dodge'
    config_scale <- scale_fill_continuous(low="#daadec", high="#5f416b")
    config_legend <- theme(legend.position = 'none')
    config_fill <- response
    config_tooltip_title <- x_var
  } else {
    config_position <- position_dodge(width = 0.9)
    config_scale <- scale_fill_brewer(palette= color_palette)
    config_legend <- theme(legend.position = 'right')
    config_fill <- facet_var
    config_tooltip_title <- facet_var
  }
  
  # Create tooltip
  if(max(na.omit(tbl[[response]])) >= 100000) {
    tbl$tooltip <- paste0(
      '<b>', tbl[[config_tooltip_title]], '</b><br>',
      formatC(tbl[[response]], format="E", 2),' &plusmn; ', formatC(tbl[[variance]], format="E", 2)
    )
  } else {
    tbl$tooltip <- paste0(
      '<b>', tbl[[config_tooltip_title]], '</b><br>',
      round(tbl[[response]],3),' &plusmn; ', round(tbl[[variance]],3)
    )
  }
  
  # Initiate ggplot object
  g <- ggplot(tbl, aes_string(x_var, response, fill = config_fill, group = facet_var))
  
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
  g <- g + labs(x = factors_label[[x_var]], y = response_label)
  g <- g + theme_minimal()
  #g <- g + ggtitle(paste0(response_label,'by\n',paste0(unlist(factors_label),collapse = ' &\n')))
  g <- g + theme(plot.title = element_text(hjust = 0.5))
  g <- g + theme(strip.text.y = element_text(angle = 0))
  g <- g + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  g <- g + config_legend
  if(flip_coord) g <- g + coord_flip()
  
  ggiraph(code = print(g), hover_css = "opacity: 0.5;stroke: #ffec8b; cursor: crosshair;")
  
}
