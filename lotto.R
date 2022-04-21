
library(shiny)
library(shinydashboard)
ui <-dashboardPage(
  dashboardHeader(title = 'Lotto'),
  dashboardSidebar(
    textInput('usernumbers', 'Your numbers'),
    actionButton('go', 'play')
  ),
  dashboardBody(
    textOutput('result')
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$go,{
    # strsplit(input$usernumbers, ',')
    your_numbers <- as.numeric(strsplit(input$usernumbers, ',')[[1]])
    game_numbers <- sample(1:90, 5)
    print(your_numbers)
    print(game_numbers)
    
    # sum(your_numbers%in%game_numbers)
    
    output$result<- renderText({
      paste('Your numbers: ', paste(your_numbers, collapse = ", "), '\n',
                 'Random numbers: ', paste(game_numbers, collapse = ", "), '\n', 
                 'You found: ',  sum(your_numbers%in%game_numbers))    })
  })
  
}

shinyApp(ui, server)
#