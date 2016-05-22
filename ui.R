shinyUI(pageWithSidebar(
  headerPanel('Unlimited networks'),
  sidebarPanel(
    sliderInput('thresh', 'Threshold distance (m)', 15,
                min = 8, max = 45, step = 0.1)
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Edges Table", h4("Edges of the network"), tableOutput('table1')),
                tabPanel("Nodes Table", h4("Nodes of the network"), tableOutput('table2')),
                tabPanel("Network Plots", h4("Upper plot controls lower plot - brush to zoom in"), plotOutput('plot1', brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
                         "", plotOutput('plot2')),
                tabPanel("Summary Plots", plotOutput('plot3'),
                         "", plotOutput('plot4'),
                         "", plotOutput('plot5'),
                         "", plotOutput('plot6'))
                
    )
  )
))
