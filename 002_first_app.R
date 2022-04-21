
# addig different type of ui widgets
# https://shiny.rstudio.com/gallery/widget-gallery.html

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hello shiny"),
  sliderInput('my_num', label = 'choose a number', min = 0, max = 10,step = 0.5, value = 5),
  textInput('my_text', label = 'textinput',value = 'asd', placeholder = 'write here', width = '80%'),
  br(),
  verbatimTextOutput('my_info_text')
  #textOutput('my_info_text') # try with textoutput
  
  #
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$my_info_text <- renderPrint({
    return(rep(input$my_text, round(input$my_num,0)) )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

