#
source("global_utils.R", local = TRUE) #global functions available for the whole session

function(input, output, session) {
  
  # Source all files from server_files directory and subdirectories
  files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f, local = TRUE)
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

}
