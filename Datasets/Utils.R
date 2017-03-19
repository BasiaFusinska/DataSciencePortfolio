multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

library(ggplot2)

draw.hist <- function(data, aes, xlabel, bins=20) {
  ggplot(data, aes) +
    geom_histogram(stat = "bin", bins=bins, aes(fill=..count..)) +
    xlab(xlabel) + 
    theme(legend.position="none")
}

split.dataset <- function(data, split.ratio, seed=123){
  smp_size <- floor(split.ratio * nrow(data))
  
  ## set the seed to make your partition reproductible
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  return(list(train = data[train_ind, ], test = data[-train_ind, ]))
}

