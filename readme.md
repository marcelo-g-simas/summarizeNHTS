"An open-source, survey-specific toolkit capable of processing weighted data, computing common statistics and errors, and producing interactive web visualizations will increase transparency in analyses, eliminate common technical stumbling blocks, and ultimately attract more interest to these powerful datasets." - NHTS.summarizer Team

To read more about the National Household Travel Survey, visit this [documentation page](http://nhts.ornl.gov/documentation.shtml).

# Data

This package handles the downloading, organizing, and loading of NHTS datasets for you. It reads directly from the Oak Ridge National Laboratory NHTS [data page](http://nhts.ornl.gov/download.shtml), and currently supports the 2001 and 2009 studies. The 2016 study will be immediately supported once the data is released.

# Install

We recommend using [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/) to develop analyses with this package. Your computer should have at least 8GB of memory and a recent [64 bit version of R](https://cran.r-project.org/).

```R
install.packages('devtools')
devtools::install_github('Westat-Transportation/NHTS.summarizer')
```
# Demo

```R
library(NHTS.summarizer)
download_nhts_data("2009", "C:/NHTS")
data_trip <- read_nhts_data("2009", c("FLAG100", "R_SEX", "WHYTO"), "C:/NHTS")
triprate_by_sex <- make_table(
	data = data_trip, 
	agg = 'person_trip_rate',
	factors = c("R_SEX"),
	subset = "FLAG100 == 1 & R_SEX %in% c(1,2)"
)
triprate_by_sex %>%
  make_bar_chart()
```

# Extended Demo (or Vignette)

**A new demo is coming very soon - please stay tuned!** For now, visit the [old demo](https://rawgit.com/Westat-Transportation/NHTS-Summarizer/master/demo.html) to see how you can develop complex and interactive tables, charts, and maps using data from the largest household travel survey in the United States.