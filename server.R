library(shiny)
library(ggplot2)
library(plotly)

myCsv <- read.csv("/Users/achimnyswallow/Documents/Shiny/Azteca_incidence_s06_Shiny.csv")

scale <- subset(myCsv, scale_incidence == "1")
nest <- myCsv$Tag

nests_xy <- matrix(cbind(myCsv$Xplot,myCsv$Yplot),ncol=2)
dist <- as.matrix(dist(nests_xy))

## create a matrix that contains nest to nest distance value
dist <- as.matrix(dist(nests_xy))  ## dist is a function that calculates the distance of every two trees
diag(dist) <- 0  ## diagnol of the distance matrix is 0 - because self-to-self equals to 0

modiinfo <- function(d) {
  DM <- matrix(0, nrow = nrow(dist), ncol = ncol(dist))
  DM <- dist[] < d
  diag(DM) = 0
  I <- diag(1, nrow=nrow(dist), ncol=ncol(dist))
  D <- matrix(0, nrow=nrow(dist), ncol = ncol(dist))
  D <- sapply(1:nrow(dist), function(z) DM[,z]/(z+1))
  inv <- solve(I-D)
  
  library(igraph)
  identity <- function(x) myCsv$Tag[x]
  x_cord <- function(x) myCsv$Xplot[x]
  y_cord <- function(x) myCsv$Yplot[x]
  cmtsize <- function(x) length(which(inv[,x]!=0))
  comp <- function(x) which(inv[,x]!=0)[1]
  g <- graph.adjacency(DM)
  between <- function(x) betweenness(g, directed=FALSE)[x]
  close <- function(x) closeness(g, mode = "out")[x]
  degree_centrality <- function(x) length(which(DM[,x]==1))
  scale <- function(x) myCsv$scale_incidence[x]
  predator <- function(x) myCsv$beetle_incidence[x]
  fungus <- function(x) myCsv$fungus_incidence[x]
  tree <- function(x) myCsv$especie[x]
  
  
  
  node_prop <- list()
  N <- nrow(dist)
  for (i in 1:N) {
    node_prop[[i]] = data.frame(identity = identity(i), compartment = comp(i), cmtsize = cmtsize(i), x_cord = x_cord(i), y_cord = y_cord(i), compartment_size = cmtsize(i), betweenness_centrality = between(i), closeness_centrality = close(i), degree_centrality = degree_centrality(i), mutualism = scale(i), predator = predator(i), fungus = fungus(i), tree_species = tree(i)) 
  }
  
  nodeproperties <- do.call(rbind, node_prop)
  #return(nodeproperties)
  ## delete nodes within distance threshold and clusters they belong to 
  #return(nodeproperties$compartment)
  
  dtcompartments <- nodeproperties$compartment[which(nodeproperties$x_cord < d)] ## this is a test that needs to be deleted later
  unidt <- unique(dtcompartments)
  #return(unidt)
  
  p1 <- nodeproperties$compartment[which(nodeproperties$x_cord < 0 & nodeproperties$y_cord < 100+d)] 
  p2 <- nodeproperties$compartment[which(nodeproperties$y_cord > 600-d)]
  p3 <- nodeproperties$compartment[which(nodeproperties$y_cord < d)]
  p4 <- nodeproperties$compartment[which(nodeproperties$x_cord < -300+d)]
  p5 <- nodeproperties$compartment[which(nodeproperties$x_cord >  500-d)]
  p6 <- nodeproperties$compartment[which(nodeproperties$x_cord < d & nodeproperties$y_cord < 100)]
  
  special <- subset(nodeproperties, nodeproperties$x_cord > 0 & nodeproperties$x_cord < d & nodeproperties$y_cord > 100 & nodeproperties$y_cord < 100+d)
  
  #special_dist <- sqrt((special$x_cord - 0)^2 + (special$y_cord - 100)^2)
  #return(special_dist)
  
  p7 <- nodeproperties$compartment[which(sqrt((special$x_cord - 0)^2 + (special$y_cord - 100)^2) < d)]
  
  unidt1 <- unique(p1) 
  unidt2 <- unique(p2)
  unidt3 <- unique(p3)
  unidt4 <- unique(p4)
  unidt5 <- unique(p5)
  unidt6 <- unique(p6)
  unidt7 <- unique(p7)
  
  unidt <- unique(c(unidt1, unidt2, unidt3, unidt4, unidt5, unidt6, unidt7))
  #return(unidt)
  
  library(Hmisc) #import Hmisc for dropping rows to be deleted due to compartments
  filtered <- subset(nodeproperties, compartment %nin% unidt) # delete rows
  #return(filtered) ## these three lines work
  
  ##now extract needed points, create edges and make a plot
  edg_from <- function(x) replicate(length(which(DM[,x]!=0)),x)
  edg_to <- function(x) which(DM[,x]!=0)
  X_cord_from <- function(x) replicate(length(edg_from(x)), myCsv$Xplot[x])
  Y_cord_from <- function(x) replicate(length(edg_from(x)), myCsv$Yplot[x])
  X_cord_to <- function(x) myCsv$Xplot[which(DM[,x]!=0)]
  Y_cord_to <- function(x) myCsv$Yplot[which(DM[,x]!=0)]
  
  edg_f <- list()
  N <- nrow(dist)
  for (i in 1:N) {
    edg_f[[i]] = data.frame(edg_from = edg_from(i), edg_to = edg_to(i), X_cord_from = X_cord_from(i), Y_cord_from = Y_cord_from(i), X_cord_to = X_cord_to(i), Y_cord_to = Y_cord_to(i)) 
  }
  
  edges <- do.call(rbind, edg_f)
  
  label <- c(1:nrow(dist))
  id <- c(1:nrow(dist))
  
  tt <- function(x) length(which(inv[,x]!=0))
  cmtsize <- sapply(1:nrow(dist), tt)
  zz <- function(x) which(inv[,x]!=0)[1]
  comp <- sapply(1:nrow(dist), zz)
  
  vertices <- data.frame(id, label, comp, cmtsize)
  
  df <- list(edges = edges, vertices = vertices)
  
  dfnet <- merge(df$edges, df$vertices, by.x = "edg_from", by.y = "label", all = TRUE)
  dfnet <- dfnet[complete.cases(dfnet),]
  
  filtered_l <- subset(dfnet, comp %nin% unidt) 
  
  return(filtered_l)
}


shinyServer(function(input, output, session) {
  
  data <- function() {
    modiinfo(as.numeric(input$thresh))
  }
  
  thresh <- reactive({
    modiinfo(input$thresh)
  })

  output$plot1 <- renderPlot({
    
    p <- ggplot(data()) + geom_point(aes(x=X_cord_from, y = Y_cord_from))  + scale_x_continuous(limits = c(-300, 500)) + scale_y_continuous(limits = c(0, 600)) + labs(x = "X-cordinate (m)", y = "Y-cordinate (m)")
    q <- p + geom_segment(data=data(), aes(x = X_cord_from, y = Y_cord_from, xend = X_cord_to, yend = Y_cord_to), color="pink", alpha = 0.3)  
    q
    })
    
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(data(), aes(x = X_cord_from, y = Y_cord_from)) + geom_point() + geom_segment(data=data(), aes(x = X_cord_from, y = Y_cord_from, xend = X_cord_to, yend = Y_cord_to), color="pink", alpha = 0.8)  + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + labs(x = "X-cordinate (m)", y = "Y-cordinate (m)")
    
    })
  
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  output$table1 <- renderTable({
    data()
  })
})
