# -------------- Read data at start up
#
source("data/datascope_api.R", local = TRUE)

# Datascope URLs
country_url <- "http://datascope.amida-demo.com/countries/"
indicator_url <- "http://datascope.amida-demo.com/indicators/"
tsne_url <- "http://distill.pub/2016/misread-tsne/"

# ---------------------------------
# tsne pre-calculated cloud of points
tsne_points <- read.csv("data/tsne_points.csv",stringsAsFactors = FALSE)

