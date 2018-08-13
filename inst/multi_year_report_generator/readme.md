This is a program for generating a report that visualizes the statistical significance of specified National Household Travel Survey estimates across years. The proof of concept available here can be [previewed online](https://rawgit.com/Westat-Transportation/summarizeNHTS/master/inst/multi_year_report_generator/report.html) and rendered yourself using the instructions below.

The report computes interval estimates at a 95% confidence level and visualizes significant changes from 2001 to 2017 using a crosstab table visualization with nested line charts. Separately, interactive bar charts for each year's estimates are available for review and magnification. Learn more about the background and methodology for this report by reviewing a [poster](Fucci,A;Cates,A.pdf) created for the 2018 National Household Travel Survey Workshop.

You can use the provided text configuration file, estimates.csv, to specify your own statistics. Before tinkering with configuring estimates, we recommend you follow these steps to reproduce this demo report on your own computer:

1. Verify that you have summarizeNHTS [installed](https://github.com/Westat-Transportation/summarizeNHTS/tree/master/inst/install).
```R
library(summarizeNHTS)
```

2. Make sure you have downloaded 2001, 2009 and 2017 data.
```R
download_nhts_data(dataset="2001", exdir="C:/NHTS")
download_nhts_data(dataset="2009", exdir="C:/NHTS")
download_nhts_data(dataset="2017", exdir="C:/NHTS")
```

3. Copy the files in this directory to a local directory on your computer (for example, "C:/NHTS/multi_year_report_generator")
	- estimates.csv
	- process-estimates.R
	- create-trend-table.R
	- report.Rmd
	- derived variable files (folder)

4. Copy each derived variable file from the "derived variable files" directory to its respective year csv data directories (for example, "C:/NHTS/csv/2017" for 2017 if the download_nhts_data() directory specified was "C:/NHTS")
	- derived variables files
		- 2001, derived_variable_config.csv
		- 2009, derived_variable_config.csv
		- 2017, derived_variable_config.csv
		
5. There are two ways to render the report.
	- Option 1: Run the following code. That's it. Note that "input" is set to the location of report.Rmd, and "csv_file_location" is set to the directory specified in download_nhts_data() when you downloaded the data.
```R
rmarkdown::render(
  input = "C:/source/summarizeNHTS/inst/multi_year_report_generator/report.Rmd",
  params = list(
    csv_file_location = "C:/NHTS"
  )
)
```
	- Option 2: In Rstudio, open report.Rmd and select the "Knit" button. That's it. Note that "csv_file_location" on line 6 of report.Rmd should be set to the directory specified in download_nhts_data() when you downloaded the data.