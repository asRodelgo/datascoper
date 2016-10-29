# -------- Read Datascope API
#
library(jsonlite)
library(dplyr)
# Query country metadata:
countries <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/countries/?fields=id%2Ciso2%2Ciso3%2Cname%2Cregion%2CincomeLevel%2ClendingType%2CcapitalCity%2Cgeo",
                      flatten = TRUE)
# Query indicators:
indicators <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/indicators?fields=id%2Cname%2Cdataset%2CvalueType%2CdatasetId%2Cnotes%2Cproperties%2Crank%2Cdefinition",
                      flatten=TRUE)
# Data from ranks 1-2 for all countries
types_allowed <- c("value","percent_of_gdp","dtf","usd_millions","us_dollars","percent")

indicators_1_2 <- indicators %>%
  filter(rank < 3, valueType %in% types_allowed) %>%
  distinct(name,.keep_all=TRUE) %>%
  arrange(name)

# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame

for (cou in ){
  for (){
    thisQuery <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/data?countries=BRA&indicators=2760",
                          flatten = TRUE)
    thisQuery <- flatten(thisQuery$data$indicators[[1]])
    thisQuery <- select(thisQuery, -estimated)
    names(thisQuery) <- gsub("values.","",names(thisQuery),fixed=TRUE)
    
  }
}


#test2 <- flatten(test$data)





