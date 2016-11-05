# --------- Filter and processing functions for plots

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_filter <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  # transform indicators text into codes (for now, add/remove X)
  #selected_indicators <- paste0("X",selected_indicators)
  # 
  if (length(tsne_ready)>0){ # if data do stuff
    if (!is.null(selected_indicators)){  # if at least 1 selected indicator
      # map indicator labels to codes
      selected_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
      #
      tsne_ready_select <- tsne_ready %>%
        dplyr::select(iso3, Period, Region, 
                      IncomeLevel, Country, x, y, 
                      one_of(selected_indicators))
    } else { # no selected indicators    
      tsne_ready_select <- tsne_ready %>%
        dplyr::select(iso3, Period, Region, 
                      IncomeLevel, Country, x, y)
    }  
    # General Filters
    tsne_points_filter <- tsne_ready_select %>%
      filter(Country %in% colCountry & Region %in% colRegion & 
               Period %in% colPeriod) %>%
      group_by(Country,Period) %>%
      mutate(group = ifelse(length(colRegion)>2,
                            ifelse(length(colPeriod) == 2,
                                   ifelse(length(colCountry)>2,Period,paste0(Country," (",Period,")")),
                                   ifelse(length(colCountry)>2,Region,
                                          ifelse(length(colPeriod)==1,paste0(Country," (",Period,")"),Country))),
                            ifelse(length(colPeriod)>2,ifelse(length(colCountry)>2,Region,Country),
                                   ifelse(length(colCountry)>2,paste0(Region," (",Period,")"),paste0(Country," (",Period,")")))))
    tsne_points_filter <- as.data.frame(tsne_points_filter)
    tsne_points_filter_out <- tsne_ready_select %>%
      filter(!(Country %in% colCountry & Region %in% colRegion & Period %in% colPeriod))
    
    
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
  
  # transform indicators text into codes (for now, add/remove X)
  #selected_indicators <- paste0("X",selected_indicators)
  # 
  if (length(tsne_ready)>0){ # if data do stuff
    # map indicator labels to codes
    selected_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
    #
    tsne_ready_select <- tsne_ready %>%
      dplyr::select(iso3, Period, Region, 
                    IncomeLevel, Country, x, y, 
                    one_of(selected_indicators))
    
    # General Filters
    tsne_points_filter <- tsne_ready_select %>%
      filter(Country %in% colCountry & Region %in% colRegion & Period %in% colPeriod)
    tsne_points_filter_out <- tsne_ready_select %>%
      filter(!(Country %in% colCountry & Region %in% colRegion & Period %in% colPeriod))
    
  } else{ return()}
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

