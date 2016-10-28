# -------------- Read data at start up
#
# TCMN data section: REPLACE WITH DATASCOPE API
TCMN_data <- read.csv("data/TCMN_data.csv", colClasses = c(rep("character",4),rep("numeric",2),rep("character",2)))

# country table ----------------------------
countries <- read.csv("data/CountryClassification.csv", stringsAsFactors = FALSE)
# Avoid ISO2 code for Namibia be confused by NA
countries[countries$CountryCodeISO3=="NAM",]$CountryCodeISO2 <- "NA"
countries <- arrange(countries, Country)

# list of only countries (useful for selectors and others)
countryNames <- filter(countries, !(CountryCodeISO2==""))
countryNames <- select(countryNames, CountryCodeISO3, Country)# remove CountryISO2

# ---------------------------------
# tsne pre-calculated cloud of points
tsne_points <- read.csv("data/tsne_points.csv",stringsAsFactors = FALSE)

