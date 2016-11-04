# -------- Plot tSNE-based cloud of points

.tSNE_plot_All <- function(colRegion,colPeriod,colCountry,colIndicator,centralMeasure="mean",showLabels=FALSE){
  # tsne_points contains pairs of coordinate points to plot
  # Parameters -----------
  # 
  #colPeriod <- "All"
  #colCountry <- "All" 
  #colRegion <- "All"
  #colIndicator <- "All" 
  # ----------------------
  #
  # ------------------------------------
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  #
  if (length(tsne_ready)>0){ # if data do stuff
    par(mar=c(0,0,0,0))
    
    tsne_ready_plot <- tsne_ready %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
    
    # General Filters
    tsne_points_filter <- tsne_ready_plot %>%
      filter(Country %in% colCountry & Region %in% colRegion 
             & Period %in% colPeriod) %>%
      group_by(Country,Period) %>%
      mutate(group = ifelse(length(colRegion)>2,
                            ifelse(length(colPeriod) == 2,
                                   ifelse(length(colCountry)>2,Period,paste0(Country," (",Period,")")),
                                   ifelse(length(colCountry)>2,Region,
                                          ifelse(length(colPeriod)==1,paste0(Country," (",Period,")"),Country))),
                            ifelse(length(colPeriod)>2,ifelse(length(colCountry)>2,Region,Country),
                                   ifelse(length(colCountry)>2,paste0(Region," (",Period,")"),paste0(Country," (",Period,")")))))
    
    
    centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    
    tsne_points_filter_out <- tsne_ready_plot %>%
      filter(!(Country %in% colCountry & Region %in% colRegion 
               & Period %in% colPeriod))
    # Skills filter
    if (!(colIndicator=="All")){
      
      if (centralMeasure=="mean"){
        ggplot(NULL, aes(x,y)) +
          geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colIndicator))),size=2) +
          scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colIndicator)))), low="blue", mid="white",high="red")+
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
          theme(#legend.key=element_blank(),
                legend.title=element_blank(),
                legend.text = element_text(size = 10),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      } else{
        ggplot(NULL, aes(x,y)) +
          geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colIndicator))),size=2) +
          scale_color_gradient2(midpoint=median(eval(parse(text=paste0("tsne_points_filter$",colIndicator)))), low="blue", mid="white",high="red")+
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.text = element_text(size = 10),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      }
      
    } else {
      
      if (showLabels){ # show names and year of countries
        ggplot(NULL, aes(x,y)) +  
          #geom_point(data=tsne_points_filter,aes(group=Country,color = Country),size=2) +
          geom_point(data=tsne_points_filter,aes(group=group,color = group),size=2) +
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
          geom_point(data=centroid,color="red",size=3) + 
          geom_text(data=tsne_points_filter,aes(label=str_wrap(paste0(Country," (",Period,")"))),color="grey",nudge_y=0.1)+
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.text = element_text(size = 15),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      } else {
        ggplot(NULL, aes(x,y)) +  
          geom_point(data=tsne_points_filter,aes(group=group,color = group),size=2) +
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
          geom_point(data=centroid,color="red",size=3) + 
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.text = element_text(size = 15),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      }
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}