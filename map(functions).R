library(ggplot2)
library(rgeos)
library(maptools)

fortify_spatial_data_frame <- function(x, id_field_name) {
	if(missing(id_field_name)) {
    id <- "id"
  } else {
    names(x)[which(names(x) == id_field_name)] <- "id"
  }
	x@data$id <- rownames(x@data)
	x.points <- fortify(x, region="id")
	x.df <- merge(x.points, x@data, by="id")
	return(x.df)
}