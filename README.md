[![DOI](https://zenodo.org/badge/50800648.svg)](https://zenodo.org/badge/latestdoi/50800648)

# ulnet
Generate an unlimited number of networks based on threshold distance.

### Motivation
I began this project to fulfill my research need of generating testable networks based on spatial data. I had nodes with 2D spatial coordinates but did not know whether there was an edge between any two nodes. It's unfortunate that I failed to find available tools allowing me to generate as many networks as I wanted, as most of them did not have a spatial component.  To solve this problem, I began this coding project in which the application of the fundamental matrix of Markov Chain is the spirit.   

### Features of the product
1. The application of the fundamental matrix of Markov Chain to generate as many networks as a user wishes based on user-defined threshold distance.
2. Appropriate for networks with spatial information. 
3. Applied to nodes with spatial coordinates with or without spatial boundaries.

#### The shiny app
server.R and ui.R are essential for the production of the shiny app, which you can find at https://hhsieh.shinyapps.io/Ulnet/
Users can manipulate the slider on the left side of the app page. A node table, an edge table, a network plot, and summary plots providing histograms of compartment size, degree centrality, betweenness centrality and closeness centrality will appear as the user decides the threshold distance on the slider. As an ecologist, I am also concerned about other node information. This includes tree species, the occurrence of insect pathogen, and the occurrence of species interactions such as mutualism and predation. 


#### This_works.R
This script allows users to produce network information and visualize networks on the desktop, a special case of a polygon plot is applied.

#### noplotboundary.R
This script includes two functions, nb and nb_exp. Both functions are applicable to networks with no spatial boundaries, as the former uses geometric threshold distance and the latter uses negative exponential function of geometric distance to generate networks. 

#### rectangular.R
This script includes two functions, recnet and recnet_exp. Both functions are applicable to networks within rectangular plots. The former uses gemoetirc threshold distance and the latter uses negative exponential function of geometric distance to generate networks. 

### Project ongoing
Currently working on an R package to ease the applications of the functions for a braoder set of users.

### License 
This is a GNU GPLv3 licensed product

### Stay tuned!
