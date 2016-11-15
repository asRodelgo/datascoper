#
source("global.R", local = TRUE) #global functions available for the whole session

function(input, output, session) {
  
  # Plot tSNE ---------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plotTSNE <- renderPlot({
    plotTSNE <- .tSNE_plot_All(input$colRegion,input$colPeriod,input$colCountry,input$colIndicator)
    #,input$centralMeasure)
    if (!is.null(ranges$x)){
      plotTSNE <- .tSNE_plot_All(input$colRegion,input$colPeriod,input$colCountry,input$colIndicator,showLabels=TRUE)+ 
        coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    } else {
      plotTSNE <- plotTSNE + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    }
    #Sys.sleep(2)
    return(plotTSNE)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # tooltip hover over scatterplot points: see https://gitlab.com/snippets/16220
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(.tSNE_plot_filter_hover(input$colRegion,input$colPeriod,input$colCountry,
                                                input$explore_variables),
                        hover, threshold = 4, maxpoints = 1)
    
    if (nrow(point) == 0) return(NULL)
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    # avoid overlapping with other objects by keeping the tooltip inside the frame
    if (left_pct > .75){
      if (top_pct >.75){
        left_px <- -15*hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      } else {
        left_px <- -15*hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      }
    } else {
      
      if (top_pct >.75){
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      } else{
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      }
    }
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    panel_input <- ""
    for (i in 1:length(input$explore_variables)){
      panel_input <- paste0(panel_input,input$explore_variables[i],": ",eval(parse(text=paste0("point$X",filter(indicators_1_2, name==input$explore_variables[i])$id))),"<br/>")
    }
#     for (i in 1:length(input$explore_variables)){
#       panel_input <- paste0(panel_input,point$Indicator[i],": ",point$Observation[i],"<br/>")
#     }
    
    wellPanel(
      style = style,
      p(HTML(paste0(point$Country," - ",point$Period,"<br/><br/>",
                    "<div class='text' style='color:grey; font-size:12px;'>",panel_input,"</div>")))
    )
  })
  
  # tooltip click over scatterplot points: see https://gitlab.com/snippets/16220
  output$click_info <- renderUI({
    click <- input$plot_click
    point <- nearPoints(.tSNE_plot_filter_hover(input$colRegion,input$colPeriod,input$colCountry,
                                                input$explore_variables),
                        click, threshold = 4, maxpoints = 1, addDist = TRUE)
#     
    if (nrow(point) == 0) return(NULL)
    # calculate top 10 closest Country,Period pairs to the clicked one
    tableTop10 <- .compare10_click(point$Period,point$Country)
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (click$x - click$domain$left) / (click$domain$right - click$domain$left)
    top_pct <- (click$domain$top - click$y) / (click$domain$top - click$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    # avoid overlapping with other objects by keeping the tooltip inside the frame
    if (left_pct > .75){
      if (top_pct >.75){
        left_px <- -15*click$range$left + left_pct * (click$range$right - click$range$left)
        top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)
      } else {
        left_px <- -15*click$range$left + left_pct * (click$range$right - click$range$left)
        top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)
      }
    } else {
      
      if (top_pct >.75){
        left_px <- click$range$left + left_pct * (click$range$right - click$range$left)
        top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)
      } else{
        left_px <- click$range$left + left_pct * (click$range$right - click$range$left)
        top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)
      }
    }
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    # actual tooltip created as wellPanel
    panel_input <- paste0('Closest 10 Economies (Eucl. dist.) to ','<a href=',country_url,filter(countries, name==tableTop10$Country[1])$iso3,' target="_blank" >',tableTop10$Country[1],' (',tableTop10$Period[1],')</a><br/><br/>')
    for (i in 2:11){
      panel_input <- paste0(panel_input,'<a href=',country_url,filter(countries, name==tableTop10$Country[i])$iso3,' target="_blank" >',tableTop10$Country[i],' (',tableTop10$Period[i],')</a> : ',round(tableTop10$dist[i],3),'<br/>')
    }
    
    wellPanel(
      style = style,
      p(HTML(panel_input))
    )
  })

  # tooltip click over scatterplot points: see https://gitlab.com/snippets/16220
  output$brush_info <- renderUI({
    brush <- input$plot_brush
    brushedP <- brushedPoints(.tSNE_plot_filter_brush(input$colRegion,input$colPeriod,input$colCountry
                                                      ),brush)
    #     
    if (nrow(brushedP) == 0) return(NULL)
    
    brushSummary <- .summary_brush(brushedP,input$explore_variables)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
#     left_pct <- (brush$x - brush$domain$left) / (brush$domain$right - brush$domain$left)
#     top_pct <- (brush$domain$top - brush$y) / (brush$domain$top - brush$domain$bottom)
#     
#     # calculate distance from left and bottom side of the picture in pixels
#     # avoid overlapping with other objects by keeping the tooltip inside the frame
#     if (left_pct > .75){
#       if (top_pct >.75){
#         left_px <- -15*brush$range$left + left_pct * (brush$range$right - brush$range$left)
#         top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
#       } else {
#         left_px <- -15*brush$range$left + left_pct * (brush$range$right - brush$range$left)
#         top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
#       }
#     } else {
#       
#       if (top_pct >.75){
#         left_px <- brush$range$left + left_pct * (brush$range$right - brush$range$left)
#         top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
#       } else{
#         left_px <- brush$range$left + left_pct * (brush$range$right - brush$range$left)
#         top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
#       }
#     }
    left_px <- 0
    top_px <- 0
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    # actual tooltip created as wellPanel
    panel_input <- paste0('Averages for brushed points:<br/><br/>')
    for (i in 1:length(input$explore_variables)){
      #panel_input <- paste0(panel_input,input$explore_variables[i],": ",eval(parse(text=paste0("brushedP$X",filter(indicators_1_2, name==input$explore_variables[i])$id))),'<br/>')
      panel_input <- paste0(panel_input,input$explore_variables[i],": ",filter(brushSummary,id %in% input$explore_variables[i])$Avg,"<br/>")
    }
    
    wellPanel(
      style = style,
      p(HTML(panel_input))
      #p(str(brushedP))
    )
  })
#   
  # densities for selected variables
  output$plotTSNEdensities <- renderPlot({
    
    click <- input$plot_click
    point <- nearPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry),
                        click, threshold = 3, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0){
      plotTSNEdensities <- .densityPlots(input$colRegion,input$colPeriod,input$colCountry,
                                         input$colIndicator,NULL,NULL)
    } else {
      plotTSNEdensities <- .densityPlots(input$colRegion,input$colPeriod,input$colCountry,
                                         input$colIndicator,point$Country,point$Period)
    }
    return(plotTSNEdensities)
  })
  
  # radar chart for selected indicators
  output$plotRadarBrushed <- renderPlot({
    
    brush <- input$plot_brush
    pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry),
                                   brush)
    
    plotRadarBrushed <- .radarPlot_base(pointsBrushed)
    
    return(plotRadarBrushed)
    
  })
  
  # bar chart for selected indicators
  output$plotBarchartBrushed <- renderPlot({
    
    brush <- input$plot_brush
    pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                                     input$explore_variables), brush)
    
    plotRadarBrushed <- .bar_chart(pointsBrushed,input$colRegion,input$colPeriod,input$colCountry,input$explore_variables)
    
    return(plotRadarBrushed)
    
  })
  
  # boxplots for selected indicators
  output$plotBoxplotBrushed <- renderPlot({
    
    brush <- input$plot_brush
    pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                                     input$explore_variables), brush)
#     click <- input$plot_click
#     point <- nearPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
#                                          input$explore_variables), click, threshold = 3, 
#                        maxpoints = 1, addDist = TRUE)
    
    boxplotBrushed <- .boxPlots(pointsBrushed,input$colRegion,input$colPeriod,input$colCountry,
                                input$explore_variables)#,point$Country,point$Period)
    
    return(boxplotBrushed)
    
  })
  
  # detailed table for brushed points
#   output$tableBrushed <- DT::renderDataTable({
#     brush <- input$plot_brush
#     pointsBrushed <- brushedPoints(.tSNE_plot_filter_hover(input$colRegion,input$colPeriod,input$colCountry,
#                                                            input$explore_variables), brush)
#     tableBrushed <- .brushTable(pointsBrushed,input$explore_variables)
#     return(tableBrushed)
#   },options = list(dom = 't',pageLength = 25, paging = TRUE),rownames= FALSE,escape=FALSE)
#   
  # update country selector with region selector
  observe({
    
    if (!((input$colRegion=="All") || (is.null(input$colRegion)))){
      region <- input$colRegion
      updateSelectizeInput(session, "colCountry",
                           choices=sort(unique(filter(data_tsne_sample, Region %in% region)$Country)), 
                           selected=NULL)
    }
    
  })  
  
#    output$thisperiod <- renderText(input$colPeriod)
#    output$thisregion <- renderText(input$colRegion)
#    output$thiscountry <- renderText(input$colCountry)

}
