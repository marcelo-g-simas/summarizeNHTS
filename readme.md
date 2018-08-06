"An open-source, survey-specific toolkit capable of processing weighted data, computing common statistics and errors, and producing interactive web visualizations will increase transparency in analyses, eliminate common technical stumbling blocks, and ultimately attract more interest to these powerful datasets." - summarizeNHTS Team

To read more about the National Household Travel Survey, visit this [documentation page](http://nhts.ornl.gov/documentation.shtml).

# Data

This package handles the downloading, organizing, and loading of NHTS datasets for you. It reads directly from the Oak Ridge National Laboratory NHTS [data page](http://nhts.ornl.gov/download.shtml), and currently supports the 2001, 2009, and 2017 surveys.

# Install

Reference the [install readme](https://github.com/Westat-Transportation/summarizeNHTS/tree/master/inst/install) for specific instructions. We recommend using [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/) to develop analyses with this package. Your computer should have at least 8GB of memory and a recent [64 bit version of R](https://cran.r-project.org/).

```R
install.packages('devtools')
devtools::install_github('Westat-Transportation/summarizeNHTS')
```
# Demo

```R
library(summarizeNHTS)
download_nhts_data("2017", exdir="C:/NHTS")
dataset <- read_data("2017", csv_path="C:/NHTS")
statistic <- summarize_data(
    data = dataset,
    agg = "household_count",
    by = c("HHSIZE","HHVEHCNT")
)
make_chart(statistic)
```

# Extended Demo

Review our tutorial workshop, [Exploring the NHTS in R](https://rawgit.com/Westat-Transportation/summarizeNHTS/master/inst/tutorials/workshop/Workshop.html), for more guidance on using this package.
