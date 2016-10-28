# load global packages ----------------------------------------------
library(dplyr) # manipulate data 
library(ggplot2) # charts
library(data.table) # fast operations
library(tidyr) # transform data
library(stringr) # manipulate strings
require(tsne) # t-SNE algorithm
library(DT) # customize dataTable javascript library
library(reshape2) # manipulate data
library(knitr) # generate LaTeX PDF report

# global data and functions -----------------------------------------

# Read the global data available for the whole session. Will be loaded only once
source("data/read_data.R", local = TRUE)
# These functions are called before the actual server work. They will be loaded for the
# session
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)
#source(file.path("server_files","utilities","ppcheck_names_descriptions.R"), local = TRUE)

# PDF report R scripts
source("reporting/ReportGenerator.R", local = TRUE)
#source("reporting/TCMN_charts_PDF.R", local = TRUE)

# ----------- tSNE data topology
data_tsne <- .prepare_data()
data_tsne_sample <- filter(data_tsne, Period < "2016" & Period > "2002")
tsne_ready <- cbind(data_tsne_sample,tsne_points)
names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
names(tsne_ready)[ncol(tsne_ready)] <- "y"
# Default selector choices for tsne -----------
countries_list <- sort(unique(data_tsne_sample$CountryShort))
periods_list <- sort(unique(data_tsne_sample$Period))
regions_list <- sort(unique(data_tsne_sample$RegionShort))
indicators_list <- names(data_tsne_sample)[7:ncol(data_tsne_sample)]
#
indicator_selection_plots <- c("Ease_of_Doing_Business","Control_of_Corruption","Unemployment_rate",
                               "Imports","MFN_Tariff_Simple_Average","Remittances_received_perc_of_GDP",
                               "Income_per_capita_USDollars")
indicator_selection_plots_short <- c("Ease_DB","Corruption","Unemployed",
                                     "LPI","MFN_Tariff","Remittances",
                                     "Manufac",
                                     "Export","Import","Income")
# filter TCMN_data by selected indicators in order to show the actual values on the 
# topology tab
selected_TCMN_data <- filter(TCMN_data, IndicatorShort %in% indicator_selection_plots)

# Datascope URLs
country_url <- "http://datascope.amida-demo.com/countries/"
indicator_url <- "http://datascope.amida-demo.com/indicators/"
tsne_url <- "http://distill.pub/2016/misread-tsne/"
# ---------------