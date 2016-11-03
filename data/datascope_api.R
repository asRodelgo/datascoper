# -------- Read Datascope API
datascope <- read.csv("data/datascope.csv",stringsAsFactors = FALSE)
#
# Query country metadata:
countries <- read.csv("data/countries.csv", stringsAsFactors = FALSE)
# Query indicators:
indicators <- read.csv("data/indicators.csv",stringsAsFactors = FALSE)
# Data from ranks 1-2 for all countries
types_allowed <- c("value","percent_of_gdp","dtf","usd_millions","us_dollars","percent")

indicators_1_2 <- indicators %>%
  filter(rank < 3, valueType %in% types_allowed) %>%
  distinct(name,.keep_all=TRUE) %>%
  arrange(name)



