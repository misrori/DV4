

library(shiny)

source('000_functions.R')
# get_bmi_by_index_number(23)

ui <- fluidPage(
  # Application title
  titlePanel("BMI calculator"),
  sliderInput('weight', label = 'Your weight(kg)', min = 0, max = 130,step = 1, value = 65),
  sliderInput('height', label = 'Your height(cm)', min = 100, max = 250, value = 150),
  br(),
  verbatimTextOutput('bmi_number_text'),
  textOutput('bmi_number'),
  h2('Based on your BMI You are:'),
  textOutput('bmi_text'),
  actionButton('btn', 'calculate'),
  
  tableOutput('mttabale')
  # #
)

# there are different solutions for the server side below

server <- function(input, output) {
  
  observeEvent(input$weight,{
    print(input$weight)
  })
  
  observeEvent(input$btn,{
    bmi_number <- round(input$weight / ( ((input$height)/100)^2)  ,2)
    
    output$bmi_number_text <- renderPrint({
      # bmi_number <- round(input$weight / ( ((input$height)/100)^2)  ,2)
      return(paste0('Your BMI is: ', as.character(   bmi_number   )  ))
    })
    
    
    output$bmi_number <- renderText({
      # bmi_number <- round(input$weight / ( ((input$height)/100)^2)  ,2)
      return(paste0('Your BMI is: ', as.character(   bmi_number   )  ))
      
    })
    
    output$bmi_text <- renderText({
      # get_bmi_by_index_number(round(input$weight / ( ((input$height)/100)^2)  ,2))
      return(get_bmi_by_index_number(bmi_number ))
      
    })
  })
  
  # bmi_number_reactive <- reactive({
  #   # print('something changed')
  #   # print(round(input$weight / ( ((input$height)/100)^2)  ,2))
  #   return(round(input$weight / ( ((input$height)/100)^2)  ,2))
  # })
  
  output$mttabale <- renderTable(mtcars)
  
  
}

# basic -------------------------------------------------------------------

# server <- function(input, output) {
#   
#   bmi_number_reactive <- reactive({
#     # print('something changed')
#     # print(round(input$weight / ( ((input$height)/100)^2)  ,2))
#     return(round(input$weight / ( ((input$height)/100)^2)  ,2))
#   })
#   
# 
#   output$bmi_number_text <- renderPrint({
#     # bmi_number <- round(input$weight / ( ((input$height)/100)^2)  ,2)
#     return(paste0('Your BMI is: ', as.character(   bmi_number_reactive()   )  ))
#   })
#   
#   
#   output$bmi_number <- renderText({
#     # bmi_number <- round(input$weight / ( ((input$height)/100)^2)  ,2)
#     return(paste0('Your BMI is: ', as.character(   bmi_number_reactive()   )  ))
#     
#   })
# 
#   output$bmi_text <- renderText({
#     # get_bmi_by_index_number(round(input$weight / ( ((input$height)/100)^2)  ,2))
#     return(get_bmi_by_index_number(bmi_number_reactive() ))
#     
#   })
# 
# }





# with reactive value -----------------------------------------------------

# server <- function(input, output) {
#   bmi_index <- reactive({
#     round( (input$weight / (input$height/100*input$height/100)), 2)
#   })
# 
# 
#   output$bmi_number_text <- renderPrint({
#     return(paste0('Your BMI is: ', bmi_index()  ))
#   })
# 
#   output$bmi_text <- renderText({
#     get_bmi_by_index_number(bmi_index() )
#   })
# 
# }


# with button -------------------------------------------------------------

#server <- function(input, output) {
#   observeEvent(input$btn,{
#     bmi_index <- round( (input$weight / (input$height/100*input$height/100)), 2)
# 
#     output$bmi_number_text <- renderPrint({
#       return(paste('Your BMI index is: ',   bmi_index    ) )
#     })
# 
#     output$bmi_text <- renderText({
#       get_bmi_by_index_number( bmi_index  )
#     })
# 
#   })
# 
# }



# Run the application 
shinyApp(ui = ui, server = server)

