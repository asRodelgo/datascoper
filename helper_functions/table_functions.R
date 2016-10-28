# ------ Tables 

.brushTable <- function(brushPoints,selected_indicators){
  
  if (!is.null(selected_indicators)){  
    # brushed points
    brushPoints <- dplyr::select(brushPoints,Country=CountryShort, CountryCode, Period, one_of(selected_indicators))
    #names(brushPoints) <- c("Country","Period",indicator_selection_plots_short)
    # actual data filter
    selected_TCMN_data <- .filter_TCMN_data()
    # merge
    brushPoints_actual <- merge(selected_TCMN_data,brushPoints[,c("Country","Period")], 
                                by.x = c("CountryShort","Period"), by.y = c("Country","Period"))
    brushPoints_actual <- brushPoints_actual %>%
      dplyr::select(Country=CountryShort, CountryCode,Period, one_of(selected_indicators)) %>%
      mutate(Country = paste0('<a href=',country_url,CountryCode,' target="_blank" >',Country,'</a>')) %>%
      dplyr::select(-CountryCode)
    
    require(stringr) # to wrap label text
    names(brushPoints_actual) <- gsub("_"," ",names(brushPoints_actual))
    names(brushPoints_actual) <- str_wrap(names(brushPoints_actual), width = 25)  
    
    #names(brushPoints_actual) <- c("Country","Period",indicator_selection_plots_short)
    brushPoints_actual[,c(3:ncol(brushPoints_actual))] <- round(brushPoints_actual[,c(3:ncol(brushPoints_actual))],2)
    brushPoints_actual[is.na(brushPoints_actual)] <- "..."
    
    ## Testing hrefs for indicators
    #     count_rows <- nrow(brushPoints_actual)
    #     thisRow <- 1
    #     while (thisRow <= count_rows){
    #       brushPoints_actual[thisRow,3] <- paste0('<a href=',indicator_url,'corr.scr',' target="_blank" >',brushPoints_actual[thisRow,3],'</a>')
    #       thisRow <- thisRow + 1
    #     }
    ## end of testing
    
    #return(str(brushPoints))
    brushPoints <- brushPoints_actual
  } else {
    brushPoints <- dplyr::select(brushPoints,Country=CountryShort, Period)
  }  
  return(brushPoints)  
}

