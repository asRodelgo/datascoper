# ------- Prepare data for tSNE generation (writer) and display

# Data processing
.filter_datascope_data <- function(){
  
  data_filter <- datascope %>%
    gather(Period,Observation,-iso3,-id) %>%
    inner_join(indicators_1_2, by="id") %>%
    dplyr::select(iso3,id,Period,Observation,Indicator=name) %>%
    distinct(iso3, Period, Indicator, .keep_all=TRUE) %>%
    inner_join(select(countries,iso3,Country=name,Region=region,IncomeLevel=incomeLevel),
               by="iso3")
  
#   data_merge <- mutate(data_merge, IndicatorShort = gsub(" ","_",IndicatorShort))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("(","",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub(")","",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("%","perc",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub(",","",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("$","Dollars",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub(":","",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("=","_",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("&","and",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub(".","",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("-","_",IndicatorShort,fixed = TRUE))
#   data_merge <- mutate(data_merge, IndicatorShort = gsub("/","_",IndicatorShort,fixed = TRUE))
#   data_merge <- distinct(data_merge, CountryCode, Period, IndicatorShort, .keep_all = TRUE)
  data_spread <- spread(data_filter, id, Observation)
  # remove all NA rows
  data_tsne <- data_spread[rowSums(is.na(data_spread))<ncol(data_spread[, !sapply(data_spread, is.character)]),]
  # remove all NA columns
  data_tsne <- data_tsne[,colSums(is.na(data_tsne))<nrow(data_tsne[, !sapply(data_tsne, is.character)])]
  
  return(data_tsne)
}

# Prepare data for tSNE algorithm
.prepare_data <- function(){
  
  data_tsne <- .filter_datascope_data()
  
  # calculate missing values by indicator
  num_col <- ncol(data_tsne[, !sapply(data_tsne, is.character)])
  data_missing <- data_tsne %>%
    mutate(missing_values = rowSums(is.na(.))/num_col) %>%
    dplyr::select(Country, Period, missing_values)
  
  # impute NAs by the global mean + jitter which proved to work better visually than other imputations
  names(data_tsne) <- paste0("`",names(data_tsne),"`")
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
