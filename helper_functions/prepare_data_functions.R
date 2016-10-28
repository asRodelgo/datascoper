# ------- Prepare data for tSNE generation (writer) and display

# Data processing
.filter_TCMN_data <- function(){
  
  # read my selection of indicators
  tsne_indicators <- read.csv("data/TCMN_TSNEIndicators.csv", stringsAsFactors = FALSE)
  tsne_indicators <- dplyr::select(tsne_indicators, IndicatorShort, Subsection)
  
  TCMN_data_selected <- merge(TCMN_data, tsne_indicators, by=c("IndicatorShort","Subsection"))
  data_filter <- TCMN_data_selected %>%
    filter(!grepl("M",Period) & CountryCode %in% countryNames$CountryCodeISO3 &
             !(Source %in% c("WEF_GCREP","WEO"))) %>%
    dplyr::select(CountryCode,Period,Observation,IndicatorShort) %>%
    distinct(CountryCode, Period, IndicatorShort, .keep_all=TRUE)
  
  data_merge <- merge(data_filter,countries[,c("CountryCodeISO3","RegionShort","RegionShortIncome","CountryShort")],
                      by.x="CountryCode", by.y="CountryCodeISO3")
  
  data_merge <- mutate(data_merge, IndicatorShort = gsub(" ","_",IndicatorShort))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("(","",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub(")","",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("%","perc",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub(",","",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("$","Dollars",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub(":","",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("=","_",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("&","and",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub(".","",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("-","_",IndicatorShort,fixed = TRUE))
  data_merge <- mutate(data_merge, IndicatorShort = gsub("/","_",IndicatorShort,fixed = TRUE))
  data_merge <- distinct(data_merge, CountryCode, Period, IndicatorShort, .keep_all = TRUE)
  data_spread <- spread(data_merge, IndicatorShort, Observation)
  # remove all NA rows
  data_tsne <- data_spread[rowSums(is.na(data_spread))<ncol(data_spread[,-c(1:5)]),]
  
  return(data_tsne)
}

# Prepare data for tSNE algorithm
.prepare_data <- function(){
  
  data_tsne <- .filter_TCMN_data()
  
  num_col <- ncol(data_tsne) - 5
  data_missing <- data_tsne %>%
    mutate(missing_values = rowSums(is.na(.))/num_col) %>%
    dplyr::select(CountryCode, Period, missing_values)
  
  data_tsne <- data_tsne %>%
    mutate_if(is.numeric, funs(replace(., which(is.na(.)), mean(., na.rm=TRUE) * rnorm(length(.),1,0.02))))
  data_tsne <- as.data.frame(data_tsne)
  
  # scale to [0,1] to improve tsne final shape
  maxs <- apply(data_tsne[,-c(1:5)], 2, function(x) { max(x,na.rm=TRUE)}) 
  mins <- apply(data_tsne[,-c(1:5)], 2, function(x) { min(x,na.rm=TRUE)})
  data_tsne[,-c(1:5)] <- as.data.frame(scale(data_tsne[,-c(1:5)], center = mins, scale = maxs - mins))
  # Remove NaN
  data_tsne <- data_tsne %>%
    mutate_if(is.numeric, funs(replace(., which(is.nan(.)), mean(., na.rm=TRUE) * rnorm(length(.),1,0.02))))
  data_tsne <- as.data.frame(data_tsne)
  
  data_tsne <- distinct(data_tsne, CountryCode, Period, .keep_all = TRUE)
  data_tsne <- merge(data_tsne, data_missing, by=c("CountryCode","Period"))
  data_tsne$missing_values <- as.numeric(data_tsne$missing_values)
  data_tsne[,c(6:ncol(data_tsne))] <- round(data_tsne[,c(6:ncol(data_tsne))],3)
  data_tsne <- as.data.frame(data_tsne)
  
  return(data_tsne)
}
