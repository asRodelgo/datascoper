#
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(V8)
library(htmltools)

source("global.R", local = TRUE)

function(req) { # for bookmarking to work, UI must be embedded in a function

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
  #                h4(textOutput('thisperiod')),
  #                h4(textOutput('thisregion')),
  #                h4(textOutput('thiscountry')),
                               
                 bookmarkButton(),
                 plotOutput('plotTSNE', height = "650px",
                            hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                            click = clickOpts("plot_click"),
                            brush = brushOpts("plot_brush", delay = 100, delayType = "debounce"),
                            dblclick = "plot_dblclick"),
                 uiOutput("hover_info"),
                 uiOutput("click_info")#,
                 #uiOutput("brush_info")
               ),
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput('plotBoxplotBrushed')
        ),
        column(2,
               splitLayout(cellWidths = c("66%","33%"),
                           h4("Filter by"),
                           actionLink('help_click',"Help",icon = icon("info-sign", lib = "glyphicon")),
                           bsPopover('help_click', "Help",content = paste0('<p>The cloud of points represents a projection of 40 indicators in 2 dimensions using ',
                                                               '<a href=',tsne_url,' target="_blank">tSNE</a>. ',
                                                               'Each of the points corresponds to a country and a year. Basic controls:</p>',
                                                               '<div><ul><li>hover over a point for country stats</li>',
                                                               '<li>click on a point for top 10 closest countries</li>',
                                                               '<li>brush over a group of points to filter on boxplots</li>',
                                                               '<li>brush and double click over a group of points to zoom in</li>',
                                                               '<li>use filter selectors to visualize different elements on cloud</li>',
                                                               '<li>use color selector to visualize level values for an indicator</li>',
                                                               '<li>select up to 10 indicators to display on table, tooltip and boxplot</li></div>'
               ), trigger = 'click')
               ),
               #splitLayout(cellWidths = rep("33%", 3),
               selectizeInput('colPeriod', 'Period:', choices=c("All",sort(unique(data_tsne_sample$Period))),
                              selected=NULL,multiple=TRUE,options = list(maxItems = 5,dropdownParent = 'body')),
               selectizeInput('colRegion', 'Region:', choices=c("All",sort(unique(data_tsne_sample$Region))),
                              selected=NULL,multiple=TRUE,options = list(maxItems = 3,dropdownParent = 'body')),
               selectizeInput('colCountry', 'Country:', choices=c("All",sort(unique(data_tsne_sample$Country))),
                              selected=NULL,multiple=TRUE,options = list(maxItems = 3,dropdownParent = 'body')),
               #),
               HTML('<hr style="color: purple;">'),
               h4("Color by"),
               selectizeInput('colIndicator', 'Indicator:', choices=c("All","Missing values",indicators_1_2$name),#sort(names(data_tsne_sample)[!sapply(data_tsne_sample, is.character)])),
                              options = list(dropdownParent = 'body')),
               HTML('<hr style="color: purple;">'),
               h4("Select indicators"),
               selectizeInput(
                 'explore_variables', 'Select up to 10 indicators:', choices = indicators_1_2$name,
                 multiple = TRUE, selected = indicator_selection_names, options = list(maxItems = 10)
               )
        )
      # )
  #    fluidRow(
  #      column(9,
  #              tags$img(src = "spinner.gif",
  #                       id = "loading-spinner"),
  #             DT::dataTableOutput('tableBrushed')
  #      ),
  #       column(10,
  #         br(),
  #         tags$img(src = "spinner.gif",
  #                  id = "loading-spinner"),
  #         plotOutput('plotBoxplotBrushed')
  #       )
       )  
        
    )
        
  )
}