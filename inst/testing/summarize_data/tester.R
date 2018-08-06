library(summarizeNHTS)

compare_statistics <- function(survey_year, tester_directory, read_data_csv_path) {
	
	stashed_numbers <- read.csv(file.path(tester_directory, file = "stashed_results_of_summarize_data.csv"), check.names = FALSE, stringsAsFactors = FALSE)
	#stashed_numbers <- stashed_numbers[!is.na(stashed_numbers$id), ]
	
	nhts_data <- read_data(survey_year, csv_path = read_data_csv_path)
	
	confirmed_results <- c()

	for(row_number in 1:nrow(stashed_numbers)) {  # nrow(stashed_numbers)
		summarize_data_call <- stashed_numbers[row_number, "summarize_data"]
		
		current_statistic <- eval(parse(text=summarize_data_call))
		# convert to json derived character string for comparison
		current_statistic <- format(current_statistic, scientific = FALSE)
		current_statistic <- trimws(current_statistic)
		current_statistic <- data.frame(current_statistic)
		current_statistic <- jsonlite::toJSON(current_statistic, digits = NA)
		current_statistic <- paste(current_statistic)
		
		stashed_statistic <- stashed_numbers[row_number, survey_year]

		if (identical(current_statistic,stashed_statistic)) {
			confirmed_results <- c(confirmed_results, paste0("PASSED: ", row_number))
		} else {
			stop(paste0("BE WORRIED! Current results do not match stashed results! : ", survey_year," (", stashed_numbers[row_number, "id"], ") ", stashed_numbers[row_number, "name"]))
		}
	}
	
	# write.csv(data.frame(confirmed_results), file.path(tester_directory, file = "results.csv"), row.names = FALSE)

	if (length(confirmed_results) == nrow(stashed_numbers)) {
		print(paste0(survey_year," SUCCESS! Every current result matches its stashed result."))
	} else {
		stop("BE WORRIED! Number of matching results does not match number of inputs!")
	}
	
	return(confirmed_results)

}
	
	
sanity_tester <- function(tester_directory, read_data_csv_path) {
	
	all_results <- data.frame(
		compare_statistics("2001", tester_directory, read_data_csv_path),
		compare_statistics("2009", tester_directory, read_data_csv_path),
		compare_statistics("2017", tester_directory, read_data_csv_path)
	)
	
	colnames(all_results) <- c("2001","2009","2017")
	write.csv(all_results, file.path(tester_directory, file = "results.csv"), row.names = FALSE)

}
