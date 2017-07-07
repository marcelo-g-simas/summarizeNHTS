"An open-source, survey-specific toolkit capable of processing weighted data, computing common statistics and errors, and producing interactive web visualizations will increase transparency in analyses, eliminate common technical stumbling blocks, and ultimately attract more interest to these powerful datasets." - NHTS.summarizer Team

To read more about the National Household Travel Survey, visit this [documentation page](http://nhts.ornl.gov/documentation.shtml).

# Data

This package handles the downloading, organizing, and loading of NHTS datasets for you. It reads directly from the Oak Ridge National Laboratory NHTS [data page](http://nhts.ornl.gov/download.shtml), and currently supports the 2001 and 2009 studies. The 2016 study will be immediately supported once the data is released.

# Install

```R
install.packages('devtools')
devtools::install_github('Westat-Transportation/NHTS.summarizer')
```
# Demo

```R
download_nhts_data("2009", "C:/NHTS")
dt <- read_nhts_data("2009", c("HH_HISP", "HHSIZE"), "C:/NHTS")
```

# Extended Demo (or Vignette)

**A new demo is coming very soon - please stay tuned!** For now, visit the [old demo](https://rawgit.com/Westat-Transportation/NHTS-Summarizer/master/demo.html) to see how you can develop complex and interactive tables, charts, and maps using data from the largest household travel survey in the United States.