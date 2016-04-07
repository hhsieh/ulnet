shinyUI(pageWithSidebar(
  headerPanel('Unlimited networks'),
  sidebarPanel(
    sliderInput('thresh', 'Threshold distance (m)', 23,
                min = 8, max = 45, step = 0.1)
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plots", h4("Upper plot controls lower plot - brush to zoom in"), plotOutput('plot1', brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
                         "", plotOutput('plot2')),
                tabPanel("Table", tableOutput('table1'))
    )
  )
))
