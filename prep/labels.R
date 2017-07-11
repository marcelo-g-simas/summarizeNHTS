# See prep/variables.R to get variable lists

file_labels_2001 <- file.choose()
file_labels_2009 <- file.choose()

labels_2001 <- fread(file_labels_2001)
labels_2009 <- fread(file_labels_2009)


nhts_2001 <- list(
  labels = labels_2001,
  variables = variables_2001
)

nhts_2009 <- list(
  labels = labels_2009,
  variables = variables_2009
)

#devtools::use_data(nhts_2001, nhts_2009, overwrite = TRUE)
