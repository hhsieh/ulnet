library(ggplot2)
library(RCurl)
library(igraph)
library(plotly)

myCsv <- getURL("https://docs.google.com/spreadsheets/d/181m8T_QFkUR2nuXk0fEGNcgayA-oFaU6y38TQtgpOT8/pub?output=csv")
myCsv <- read.csv(textConnection(myCsv))

nests_xy <- matrix(cbind(myCsv$Xplot,myCsv$Yplot),ncol=2)
dist <- as.matrix(dist(nests_xy))

## create a matrix that contains nest to nest distance value
dist <- as.matrix(dist(nests_xy))  ## dist is a function that calculates the distance of every two trees
diag(dist) <- 0  ## diagnol of the distance matrix is 0 - because self-to-self equals to 0

### network creation and visualization
endDM <- function(d) {
  DM <- matrix(0, nrow = nrow(dist), ncol = ncol(dist))
  DM <- dist[] < d
  diag(DM) = 0
  I <- diag(1, nrow=nrow(dist), ncol=ncol(dist))
  D <- matrix(0, nrow=nrow(dist), ncol = ncol(dist))
  D <- sapply(1:nrow(dist), function(z) DM[,z]/(z+1))
  inv <- solve(I-D)

  
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
  #return(dfnet)
  p <- ggplot(dfnet, xlab = "X-coordinate (m)", ylab = "Y-coordinate (m)") + geom_point(aes(x=X_cord_from, y = Y_cord_from)) 
  
  q <- p + geom_segment(data=dfnet, aes(x=dfnet$X_cord_from, y = dfnet$Y_cord_from, xend = dfnet$X_cord_to, yend = dfnet$Y_cord_to), color="pink", alpha = 0.1)
  ggplotly(q)
}



 


## Collect node information - various centralitie + compartment size + occurrences of species interactions

nodeinfo <- function(d) {
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
  return(nodeproperties)
}


## modify function nodeinfo to delete nodes and clusters within the margin 
ntinfo <- function(d) {
    
  #based on user-defined threshold distance, d, to create an adjancency matrix of nodes
  DM <- dist[] < d
  diag(DM) = 0
  I <- diag(1, nrow=nrow(dist), ncol=ncol(dist))
  
  #the application of the fundamental matrix of Markov Chain to generate information of comparments
  D <- matrix(0, nrow=nrow(dist), ncol = ncol(dist))
  D <- sapply(1:nrow(dist), function(z) DM[,z]/(z+1))
  inv <- solve(I-D)
  
  # collect node properties of the network
  identity <- function(x) myCsv$Tag[x] #node identities
  x_cord <- function(x) myCsv$Xplot[x] #x-coordinates of nodes
  y_cord <- function(x) myCsv$Yplot[x] #y-coordinates of nodes
  cmtsize <- function(x) length(which(inv[,x]!=0)) #compartment sizes of nodes
  comp <- function(x) which(inv[,x]!=0)[1] #compartment identities
  g <- graph.adjacency(DM) #generate a graph
  between <- function(x) betweenness(g, directed=FALSE)[x] #betweenness centralities of nodes
  close <- function(x) closeness(g, mode = "out")[x] #closeness centralities of nodes
  degree_centrality <- function(x) length(which(DM[,x]==1)) #degree centralities of nodes
  scale <- function(x) myCsv$scale_incidence[x] #the presence/absence of the protection mutualism
  predator <- function(x) myCSv$beetle_incidence[x] #the presence/absence of predation
  fungus <- function(x) myCsv$fungus_incidence[x] #the presence of disease
  tree <- function(x) myCsv$especie[x] #tree species of concerned sites
  
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
  compsize_freq <- table(compsize)
  return(list(nodes = filtered_nodes, edges = filtered_edges, compsize = compsize, compsize_freq = compsize_freq))
}


## negative exponential probability function  
ntinfo_2 <- function(d, lambda) {
  
  #based on user-defined threshold distance, d, to create an adjancency matrix of nodes
  prob <- exp(-lambda * dist) 
  DM <- rbinom(length(prob), size = 1, prob =  prob) #the problem with stochasticity is the network becomes directional
  DM <- matrix(DM, nrow = nrow(dist), ncol = ncol(dist))
  DM[lower.tri(DM)] <- (t(DM)[lower.tri(DM)] == 1)
  diag(DM) = 0
  I <- diag(1, nrow=nrow(dist), ncol=ncol(dist))
  
  #the application of the fundamental matrix of Markov Chain to generate information of comparments
  D <- matrix(0, nrow=nrow(dist), ncol = ncol(dist))
  D <- sapply(1:nrow(dist), function(z) DM[,z]/(z+1))
  inv <- solve(I-D)
  
  # collect node properties of the network
  identity <- function(x) myCsv$Tag[x] #node identities
  x_cord <- function(x) myCsv$Xplot[x] #x-coordinates of nodes
  y_cord <- function(x) myCsv$Yplot[x] #y-coordinates of nodes
  cmtsize <- function(x) length(which(inv[,x]!=0)) #compartment sizes of nodes
  comp <- function(x) which(inv[,x]!=0)[1] #compartment identities
  g <- graph.adjacency(DM) #generate a graph
  between <- function(x) betweenness(g, directed=FALSE)[x] #betweenness centralities of nodes
  close <- function(x) closeness(g, mode = "out")[x] #closeness centralities of nodes
  degree_centrality <- function(x) length(which(DM[,x]==1)) #degree centralities of nodes
  scale <- function(x) myCsv$scale_incidence[x] #the presence/absence of the protection mutualism
  predator <- function(x) myCsv$beetle_incidence[x] #the presence/absence of predation
  fungus <- function(x) myCsv$fungus_incidence[x] #the presence of disease
  tree <- function(x) myCsv$especie[x] #tree species of concerned sites
  
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
  compsize_freq <- table(compsize)
  return(list(nodes = filtered_nodes, edges = filtered_edges, compsize = compsize, compsize_freq = compsize_freq))
} 
