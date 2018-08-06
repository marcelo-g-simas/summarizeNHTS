library(summarizeNHTS)
library(data.table)

estimates <- fread("estimates.csv", na.strings = c("", "na", "NA", "n/a", "N/A"))

# enforce the title to be a primary key on the estimate table
duplicate_records <- estimates[, .N, title][N > 1, ]
if(nrow(duplicate_records) > 0) {
	estimates <- estimates[!duplicated(estimates$title), ]
	warning("Estimates cannot have matching titles. Titles must be unique. The following were removed, after the first occurence: ",
		paste0("\n",paste(duplicate_records$title, collapse="\n"))
	)
}

# function cannot assume analyst's computer can read all datasets into memory
# we will need to read in the statistics one year at a time and stash, so we can remove the object from memory
for(year in c("2001","2009","2017")) {
	
	message("Reading in ", year)
	suppressMessages(
		nhts_data <- read_data(year, "C:/NHTS")
	)
	message("Processing ", year)
	
	estimate_results <- list()
	for(estimate in 1:nrow(estimates)) {
		# cat(estimates[estimate, `title`])
		sd_agg <- estimates[estimate, `statistic name`]
		sd_agg_var <- estimates[estimate, `arithmetic variable`]
		sd_by <- estimates[estimate, `grouping variable(s)`]
		sd_prop <- estimates[estimate, `use proportion for count statistics`]
		sd_prop_by <- estimates[estimate, `grouping variable for proportion`]
		sd_subset <- estimates[estimate, `subset condition`]
		sd_label <- estimates[estimate, `use labels`]
		sd_exclude_missing <- estimates[estimate, `exclude missing values`]
		
		# make sure parameters take expected defaults if value not provided in estimate definition
		result <- summarize_data(
			data = nhts_data,
			agg = if(!is.na(sd_agg)) { tolower(sd_agg) } else { NULL},
			agg_var = if(!is.na(sd_agg_var)) { toupper(trimws(sd_agg_var)) } else { NULL},
			by = if(!is.na(sd_by)) { toupper(trimws(strsplit(sd_by, ",")[[1]])) } else { NULL},
			prop = if(!is.na(sd_prop)) { sd_prop } else { FALSE},
			prop_by = if(!is.na(sd_prop_by)) { toupper(trimws(sd_prop_by)) } else { NULL},
			subset = if(!is.na(sd_subset)) { sd_subset } else { NULL},
			label = if(!is.na(sd_label)) { sd_label } else { TRUE},
			exclude_missing = if(!is.na(sd_exclude_missing)) { sd_exclude_missing } else { FALSE}
		)
		
	estimate_results[[estimate]] <- result
			
	}
	
	#detach("package:summarizeNHTS", unload=TRUE)
	assign(paste0("results_",year), estimate_results)
	rm(estimate_results)
	rm(nhts_data)
	gc()
	
}
	
# warning("Making variables upper case because that is how they are in the data files.")


