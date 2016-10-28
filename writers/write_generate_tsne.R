# -------- Writers don't run when the app is called. They are used for pre-processing

# TSNE writer
.generateTSNE <- function(){
  
  data_tsne <- .prepare_data()
  
  data_tsne_sample <- filter(data_tsne, Period > "2002" & Period < "2016")
  
  if (nrow(data_tsne)>0){
    num_iter <- 400
    max_num_neighbors <- 100
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample[,-c(1:5)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    # add jitter
    tsne_points <- tsne_points + runif(length(tsne_points),-1,1)
    #plot(tsne_points_jit)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points.csv", row.names = FALSE)
}


