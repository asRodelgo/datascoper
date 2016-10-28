# --------- Filter and processing functions for plots

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_filter <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  
  if (length(tsne_ready)>0){ # if data do stuff
    if (!is.null(selected_indicators)){  # if at least 1 selected indicator
      tsne_ready_select <- tsne_ready %>%
        dplyr::select(CountryCode, Period, RegionShort, 
                      RegionShortIncome, CountryShort, x, y, 
                      one_of(selected_indicators))
    } else { # no selected indicators    
      tsne_ready_select <- tsne_ready %>%
        dplyr::select(CountryCode, Period, RegionShort, 
                      RegionShortIncome, CountryShort, x, y)
    }  
    # General Filters
    tsne_points_filter <- tsne_ready_select %>%
      filter(CountryShort %in% colCountry & RegionShort %in% colRegion & 
               Period %in% colPeriod) %>%
      group_by(CountryShort,Period) %>%
      mutate(group = ifelse(length(colRegion)>2,
                            ifelse(length(colPeriod) == 2,
                                   ifelse(length(colCountry)>2,Period,paste0(CountryShort," (",Period,")")),
                                   ifelse(length(colCountry)>2,RegionShort,
                                          ifelse(length(colPeriod)==1,paste0(CountryShort," (",Period,")"),CountryShort))),
                            ifelse(length(colPeriod)>2,ifelse(length(colCountry)>2,RegionShort,CountryShort),
                                   ifelse(length(colCountry)>2,paste0(RegionShort," (",Period,")"),paste0(CountryShort," (",Period,")")))))
    tsne_points_filter_out <- tsne_ready_select %>%
      filter(!(CountryShort %in% colCountry & RegionShort %in% colRegion & Period %in% colPeriod))
    
  } else{ return()}
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

# Filters for hover over tooltips ---------------------------------------------------------
.tSNE_plot_filter_hover <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  if (length(tsne_ready)>0){ # if data do stuff
    tsne_ready_select <- tsne_ready %>%
      dplyr::select(CountryCode, Period, RegionShort, 
                    RegionShortIncome, CountryShort, x, y, 
                    one_of(selected_indicators))
    
    # General Filters
    tsne_points_filter <- tsne_ready_select %>%
      filter(CountryShort %in% colCountry & RegionShort %in% colRegion & Period %in% colPeriod)
    tsne_points_filter_out <- tsne_ready_select %>%
      filter(!(CountryShort %in% colCountry & RegionShort %in% colRegion & Period %in% colPeriod))
    
  } else{ return()}
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

