library(shiny)
library(networkD3)

ui <- fluidPage(
  forceNetworkOutput('fnetwork')
)

server <- function(input, output, session) {
  data(MisLinks)
  data(MisNodes)
  output$fnetwork <- renderForceNetwork(
    forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.4, zoom = TRUE)
  )
}

shinyApp(ui, server)
# ??networkD3
# networkD3::forceNetwork()