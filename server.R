
##########################################
##                                      ##
##   Step 1 Import required packages    ##
##                                      ##
##########################################
library(shiny)
library(ggplot2)
library(RCurl)
library(igraph)

###############################
##                           ##
##   Step 2 Read in data     ##
##                           ##
###############################
data <- getURL("https://docs.google.com/spreadsheets/d/181m8T_QFkUR2nuXk0fEGNcgayA-oFaU6y38TQtgpOT8/pub?output=csv")
data <- read.csv(textConnection(data))

#####################################################
##                                                 ##
##   Step 3 Generate the distance matrix of nodes  ##
##                                                 ##
#####################################################
nests_xy <- matrix(cbind(data$Xplot,data$Yplot),ncol=2) 
dist <- as.matrix(dist(nests_xy))


#############################################################################################
##                                                                                         ##
##   Step4 Generate edge lists and node lists based on user-defined threshold distance     ##
##                                                                                         ##
#############################################################################################
modiinfo <- function(d) {
  DM <- matrix(0, nrow = nrow(dist), ncol = ncol(dist))
  
  #based on user-defined threshold distance, d, to create an adjancency matrix of nodes
  DM <- dist[] < d
  diag(DM) = 0
  I <- diag(1, nrow=nrow(dist), ncol=ncol(dist))
  
  #the application of the fundamental matrix of Markov Chain to generate information of comparments
  D <- matrix(0, nrow=nrow(dist), ncol = ncol(dist))
  D <- sapply(1:nrow(dist), function(z) DM[,z]/(z+1))
  inv <- solve(I-D)
  
  # collect node properties of the network
  identity <- function(x) data$Tag[x] #node identities
  x_cord <- function(x) data$Xplot[x] #x-coordinates of nodes
  y_cord <- function(x) data$Yplot[x] #y-coordinates of nodes
  cmtsize <- function(x) length(which(inv[,x]!=0)) #compartment sizes of nodes
  comp <- function(x) which(inv[,x]!=0)[1] #compartment identities
  g <- graph.adjacency(DM) #generate a graph
  between <- function(x) betweenness(g, directed=FALSE)[x] #betweenness centralities of nodes
  close <- function(x) closeness(g, mode = "out")[x] #closeness centralities of nodes
  degree_centrality <- function(x) length(which(DM[,x]==1)) #degree centralities of nodes
  scale <- function(x) data$scale_incidence[x] #the presence/absence of the protection mutualism
  predator <- function(x) data$beetle_incidence[x] #the presence/absence of predation
  fungus <- function(x) data$fungus_incidence[x] #the presence of disease
  tree <- function(x) data$especie[x] #tree species of concerned sites
  
  node_prop <- list()
  N <- nrow(dist)
  for (i in 1:N) {
    node_prop[[i]] = data.frame(node_identity = identity(i), x_cord = x_cord(i), y_cord = y_cord(i), compartment_id = comp(i), comp_size = cmtsize(i),
                                between_ct = between(i), close_ct = close(i), degree_ct = degree_centrality(i),
                                mutualism = scale(i), predation = predator(i), disease = fungus(i), tree_spp = tree(i)
                                ) 
  }
  nodeproperties <- do.call(rbind, node_prop)

  # based on user-defined threshold distance, remove nodes in comparmtnets of which distance to any of the plot margins < d
  p1 <- nodeproperties$compartment_id[which((-300+d) < nodeproperties$x_cord & nodeproperties$x_cord < d & 100 < nodeproperties$y_cord & nodeproperties$y_cord< 100+d)] 
  p2 <- nodeproperties$compartment_id[which(nodeproperties$y_cord > 600-d)]
  p3 <- nodeproperties$compartment_id[which(nodeproperties$y_cord < d)]
  p4 <- nodeproperties$compartment_id[which(nodeproperties$x_cord < -300+d)]
  p5 <- nodeproperties$compartment_id[which(nodeproperties$x_cord >  500-d)]

  unidt1 <- unique(p1) 
  unidt2 <- unique(p2)
  unidt3 <- unique(p3)
  unidt4 <- unique(p4)
  unidt5 <- unique(p5)
  unidt <- unique(c(unidt1, unidt2, unidt3, unidt4, unidt5))
  
  library(Hmisc) #import Hmisc for dropping rows to be deleted due to the threshold distance
  filtered_nodes <- subset(nodeproperties, compartment_id %nin% unidt)
  
  # produce an edge table
  edg_from <- function(x) replicate(length(which(DM[,x]!=0)),x)
  edg_to <- function(x) which(DM[,x]!=0)
  X_cord_from <- function(x) replicate(length(edg_from(x)), data$Xplot[x])
  Y_cord_from <- function(x) replicate(length(edg_from(x)), data$Yplot[x])
  X_cord_to <- function(x) data$Xplot[which(DM[,x]!=0)]
  Y_cord_to <- function(x) data$Yplot[which(DM[,x]!=0)]
  zz <- function(x) which(inv[,x]!=0)[1]
  comp <- sapply(1:nrow(dist), zz)
  compar <- function(x) replicate(length(which(DM[,x]!=0)), comp[x])
  
  edg_f <- list()
  N <- nrow(dist)
  for (i in 1:N) {
    edg_f[[i]] = data.frame(edge_from = edg_from(i), edge_to = edg_to(i), X_cord_from = X_cord_from(i), Y_cord_from = Y_cord_from(i), X_cord_to = X_cord_to(i), Y_cord_to = Y_cord_to(i), compartment_id = compar(i)) 
  }
  
  edges <- do.call(rbind, edg_f)
  filtered_edges <- subset(edges, compartment_id %nin% unidt) 
  filtered_edges <- filtered_edges[,c(1:6)]
  #return(filtered_edges)
  
  networkstats <- unique(filtered_nodes[,c(4,5)])
  compsize <- networkstats$comp_size

  return(list(nodes = filtered_nodes, edges = filtered_edges, compsize = compsize))

}


############################################
##                                        ##
##   Interactive visualization on shiny   ##
##       (needs to work with ui.R)        ##
##                                        ##
############################################
shinyServer(function(input, output, session) {
  
  data1 <- function() {
    modiinfo(as.numeric(input$thresh))
  }
  
  thresh <- reactive({
    modiinfo(input$thresh)
  })

  output$table1 <- renderTable({
    data1()[[2]]
  })
  
  output$table2 <- renderTable({
    data1()[[1]]
  })
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(data1()[[2]]) + geom_point(aes(x=X_cord_from, y = Y_cord_from))  + scale_x_continuous(limits = c(-300, 500)) + scale_y_continuous(limits = c(0, 600)) + labs(x = "X-coordinate (m)", y = "Y-coordinate (m)")
    q <- p + geom_segment(data=data1()[[2]], aes(x = X_cord_from, y = Y_cord_from, xend = X_cord_to, yend = Y_cord_to), color="pink", alpha = 0.8)  
    q
    })
    
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(data1()[[2]], aes(x = X_cord_from, y = Y_cord_from)) + geom_point() + geom_segment(data=data1()[[2]], aes(x = X_cord_from, y = Y_cord_from, xend = X_cord_to, yend = Y_cord_to), color="pink", alpha = 1)  + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + labs(x = "X-coordinate (m)", y = "Y-coordinate (m)")
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
  
  
  output$plot3 <- renderPlot({
    hist(data1()[[3]], breaks = (max(data1()[[3]]) - min(data1()[[3]])), main = "histogram of compartment size", xlab = "compmartment size" )
  })
  
  output$plot4 <- renderPlot({
    hist(data1()[[1]][6], main = "histogram of betweenness centrality", xlab = "betweenness centrality")
  })
  
  output$plot5 <- renderPlot({
    hist(data1()[[1]][7], main = "histogram of closeness centrality", xlab = "closeness centrality")
  })

  output$plot6 <- renderPlot({
    hist(data1()[[1]][8], main = "histogram of degree centrality", ylab = "degree centrality")
  })
})

