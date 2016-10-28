#
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(V8)

source("global.R", local = TRUE)


tagList(
  shinyjs::useShinyjs(),
  includeCSS("css/datascoper.css"),

  fluidPage(
    fluidRow(
      includeHTML(file.path("html", "datascoper_title.Rhtml")),
      #HTML('<hr style="color: purple;">'),
      column(10,
             fluidRow(
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput('plotTSNE', height = "700px",
                          hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                          click = clickOpts("plot_click"),
                          brush = brushOpts("plot_brush", delay = 100, delayType = "debounce"),
                          dblclick = "plot_dblclick"),
               uiOutput("hover_info")
             )
      ),
      column(2,
             splitLayout(cellWidths = c("66%","33%"),
                         h4("Filter by"),
                         actionLink('help_click',"Help",icon = icon("info-sign", lib = "glyphicon")),
                         bsPopover('help_click', "Help",content = paste0('<p>The cloud of points represents a projection of 40 indicators in 2 dimensions using ',
                                                             '<a href=',tsne_url,' target="_blank">tSNE</a>. ',
                                                             'Each of the points corresponds to a country and a year. Basic controls:</p>',
                                                             '<div><ul><li>hover over a point for country stats</li>',
                                                             '<li>click on a point for placement on boxplots</li>',
                                                             '<li>brush over a group of points to activate the table and placement on boxplots</li>',
                                                             '<li>brush and double click over a group of points to zoom in</li>',
                                                             '<li>use filter selectors to visualize different elements on cloud</li>',
                                                             '<li>use color selector to visualize level values for an indicator</li>',
                                                             '<li>select up to 10 indicators to display on table, tooltip and boxplot</li></div>'
             ), trigger = 'click')
             ),
             #splitLayout(cellWidths = rep("33%", 3),
             selectizeInput('colPeriod', 'Period:', choices=c("All",sort(unique(data_tsne_sample$Period))),
                            selected=NULL,multiple=TRUE,options = list(maxItems = 2,dropdownParent = 'body')),
             selectizeInput('colRegion', 'Region:', choices=c("All",sort(unique(data_tsne_sample$RegionShort))),
                            selected=NULL,multiple=TRUE,options = list(maxItems = 2,dropdownParent = 'body')),
             selectizeInput('colCountry', 'Country:', choices=c("All",sort(unique(data_tsne_sample$CountryShort))),
                            selected=NULL,multiple=TRUE,options = list(maxItems = 2,dropdownParent = 'body')),
             #),
             HTML('<hr style="color: purple;">'),
             h4("Color by"),
             selectizeInput('colIndicator', 'Indicator:', choices=c("All",sort(names(data_tsne_sample)[c(6:ncol(data_tsne_sample))])),
                            options = list(dropdownParent = 'body')),
             HTML('<hr style="color: purple;">'),
             h4("Select indicators"),
             selectizeInput(
               'explore_variables', 'Select up to 10 indicators:', choices = sort(names(data_tsne_sample)[c(6:ncol(data_tsne_sample))]),
               multiple = TRUE, selected = indicator_selection_plots, options = list(maxItems = 10)
             )
      )
    ),
    fluidRow(
      column(9,
#              tags$img(src = "spinner.gif",
#                       id = "loading-spinner"),
             DT::dataTableOutput('tableBrushed')
      ),
      column(3,
        br(),
        tags$img(src = "spinner.gif",
                 id = "loading-spinner"),
        plotOutput('plotBoxplotBrushed')
      )
    )  
      
  )
      
)
