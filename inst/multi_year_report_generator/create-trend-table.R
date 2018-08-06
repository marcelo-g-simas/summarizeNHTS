
create_trend_table <- function(estimate) {
	
	library(htmlTable)
	library(htmltools)
	
	table_list <- list(
		"2001" = results_2001[[estimate]],
		"2009" = results_2009[[estimate]],
		"2017" = results_2017[[estimate]]
	)
	
	# Extract group variable
	group_variable <- unique(unlist(lapply(table_list, function(x) attr(x, 'by'))))
	
	# Apply Margin of error to all tables
	table_list <- lapply(table_list, use_moe, confidence = 0.95)
	
	# Stack tables and create a "year" variable
	stacked_table <- rbindlist(table_list, idcol = 'year')
	
	# Calculate confidence intervals
	stacked_table[, CI_MIN := W - E]
	stacked_table[, CI_MAX := W + E]
	
	
	
	find_trend <- function(x, y) {
		if (any(is.na(c(x, y)))) 0
		else if (max(x) < min(y)) 1
		else if (min(x) > max(y)) -1 
		else 0
	}
	
	trend <- stacked_table[, {
		
		CI_2001 = c(CI_MIN[year == '2001'], CI_MAX[year == '2001'])
		CI_2009 = c(CI_MIN[year == '2009'], CI_MAX[year == '2009'])
		CI_2017 = c(CI_MIN[year == '2017'], CI_MAX[year == '2017'])
		
		t1 <- find_trend(CI_2001, CI_2009)
		t2 <- find_trend(CI_2009, CI_2017)
		
		trend_values <- cumsum(c(0, t1, t2))
		trend_values <- as.vector(scale(trend_values, scale = FALSE))
		
		list(trend = paste(trend_values, collapse = ','))
		
	}, keyby = group_variable]
	
	
	tooltip <- Reduce(merge, lapply(seq_along(table_list), function(x) {
		year <- names(table_list[x])
		merged <- merge(table_list[[x]], trend, by = group_variable)
		merged[, list(
			sprintf('%s: %s &plusmn; %s', year, format_values(W), format_values(E))
		), keyby = group_variable]
	}))
	
	tooltip <- tooltip[, list(title_tag = apply(.SD, 1, function(x) {
		paste(x, collapse = '|')
	})), keyby = group_variable]
	
	
	trend <- merge(trend, tooltip)
	
	trend[, data_id := .I]
	
	
	sparkline_span <- as.character(tags$span('%s', title = '%s', class = 'sparklines'))
	
	trend[, cell_value := sprintf(sparkline_span, title_tag, trend)]
	
	
	output_dimension <- sprintf('rep("", %s)', nrow(trend))
	f <- as.formula(paste('cbind(data_id)', paste(c(group_variable, output_dimension), collapse = '+'), sep = '~'))
	xtbl <- xtabs(f, trend, drop.unused.levels = T, exclude = NULL, na.action=na.pass)
	
	names(dimnames(xtbl))[names(dimnames(xtbl)) == output_dimension] <- 'Trend'
	
	# List of xtable dimensions
	xtbl_dim <- lapply(attr(xtbl, "dimnames"), length)
	
	#If there are less than 4 table dimensions
	if(length(xtbl_dim) < 4) {
		# Dimensions greater than the threshold are the row_vars
		row_vars <- names(xtbl_dim[xtbl_dim > 8])
		# If no row_vars greater then the threshold, then the largest dimension is the row_var
		if(length(row_vars) == 0) row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1]
	} else {
		# With 4 or more table dimensions, the two largest dimensions are the row_vars
		xtbl_dim <- xtbl_dim[names(xtbl_dim) != agg_label]
		row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1:2]
	}
	
	xtbl[xtbl == 0] <- NA
	# xtbl <- xtabs(reformulate(group_variable, response = 'data_id'), trend, drop.unused.levels = T)
	ftbl <- ftable(xtbl, row.vars = row_vars)
	
	
	for (cell in 1:nrow(trend)) {
		ftbl[which(ftbl == cell)] <- trend[data_id == cell, cell_value]
	}
	
	row.vars <- attr(ftbl, 'row.vars')
	col.vars <- attr(ftbl, 'col.vars')
	
	# Row Configuration
	if(length(row.vars) == 0) {
		rnames <- ''
		rowlabel <- names(col.vars[1])
		rgroup <- NULL
		n.rgroup <- NULL
	} else if(length(row.vars) == 1) {
		rnames <- row.vars[[1]]
		rowlabel <- ''
		rgroup <- names(row.vars)
		if (!is.null(rgroup)) {
			n.rgroup <- length(row.vars[[1]])
		} else {
			n.rgroup <- NULL
		}
	} else if(length(row.vars) == 2) {
		rnames <- rep(row.vars[[2]], length(row.vars[[1]]))
		rowlabel <- paste(names(row.vars), collapse = '<br><i>by</i><br>')
		rgroup <- row.vars[[1]]
		n.rgroup <- rep(length(row.vars[[2]]), length(row.vars[[1]]))
	} else {
		stop('Cannot specify more than 2 variables in the row position.\n',
				 'row_vars requires 2 variables for tables with 3 group variables.')
	}
	
	# Column Configuration
	if(length(col.vars) == 1) {
		header <- col.vars[[1]]
		cgroup <- names(col.vars)
		n.cgroup <- length(header)
	} else if (length(col.vars) == 2) {
		header <- rep(col.vars[[2]], length(col.vars[[1]]))
		cgroup <- rbind(NA, col.vars[[1]])
		cgroup[1,1] <- names(col.vars[1])
		n.cgroup <- array(NA, dim = dim(cgroup))
		n.cgroup[1,1] <- length(header)
		n.cgroup[2, ] <- length(col.vars[[2]])
	} else {
		stop('Cannot specify more than 2 variables in the column position.\n',
				 'col_vars requires 2 variables for tables with 3 group variables.')
	}
	
	# css.cell Configurations
	if(length(unlist(col.vars)) == 1) {
		# Possible htmlTable Bug: Does not respect css.cell matrix when only 1 column
		css.cell <- "border-left: .5px solid #e0e0e0; padding: 4px; font-size: 12;"
	} else {
		css.cell <- rbind(
			rep("background: #D7E5EF; padding-left: .5em; padding-right: .2em;", times=ncol(ftbl)),
			matrix("border-left: .5px solid #e0e0e0; padding: 4px; font-size: 12;", ncol=ncol(ftbl), nrow=nrow(ftbl))
		)
	}
	
	# Create htmlTable
	html_table <- htmlTable(
		ftbl,
		rnames = rnames,
		rowlabel = rowlabel,
		rgroup = rgroup,
		n.rgroup = n.rgroup,
		header = header,
		cgroup = cgroup,
		n.cgroup = n.cgroup,
		css.cell = css.cell,
		css.rgroup.sep = "border-top: .5px solid #cccccc;",
		col.columns = c('#f7f9fb','none'),
		padding.rgroup = paste(rep('&nbsp;',8), collapse = ''),
		css.table = "margin-top: 1em; margin-bottom: 1em; font-family: calibri; font-size: 13; max-width: 50px;"
	)
	
	html_table
	
}