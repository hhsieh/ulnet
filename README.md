[![DOI](https://zenodo.org/badge/50800648.svg)](https://zenodo.org/badge/latestdoi/50800648)

# ulnet
Generate an unlimited number of networks with an application of the fundamental matrix of Markov Chain to nodes with 2-D geodata (spatial coordinates).   

### Features of the product
1. The product applies the fundamental matrix of Markov Chain to generate networks by user-defined threshold distances or negative exponential functions of distances. 

2. Flexibly applied to spatially-coordinated nodes (networks) within or with no spatial boundaries.

#### The shiny app
server.R and ui.R are essential for the production of the shiny app, which you can find at https://hhsieh.shinyapps.io/Ulnet/.  It features a special case of networks in a polygon plot.
Users can manipulate the slider on the left side of the app page. A nodes table, an edges table, a network plot, and summary plots providing histograms of compartment size, degree centrality, betweenness centrality and closeness centrality of each node will appear as the user decides the threshold distance on the slider. This is a special real-life ecological case in which data of tree species, the instance of insect pathogen, and the occurrence of other species interactions (mutualism, parasitism and predation) are avaiable and important for analysis and, therefore, are included. Currently, the app generates networks within the polygon plot based on user-defined threshold distance only.

#### This_works.R
This script allows users to produce network information and visualize networks on the desktop. An example including data within a polygon plot as the shiny app is applied. Data for network visualization available.

#### noplotboundary.R
This script includes two functions, nb and nb_exp. Both functions are applicable to networks with no spatial boundaries. As the former uses geometric threshold distance, the latter uses negative exponential function of geometric distance to generate networks. 

#### rectangular.R
This script includes two functions, recnet and recnet_exp. Both functions are applicable to networks within rectangular plots. The former uses gemoetirc threshold distance and the latter uses negative exponential function of geometric distance to generate networks. 

### References
1. Steward, W. Probability, Markov Chains, Queues, and Simulation: The Mathematical Basis of Performance Modeling. 2009. Princeton University Press.
