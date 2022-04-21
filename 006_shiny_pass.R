#https://github.com/aoles/shinypass/blob/master/server.R


library(shiny)
library(jsonlite)
library(data.table)
library(httr)
library(rtsdata)
library(DT)
library(TTR)
library(plotly)
library(shinyjs)

source('000_functions.R')

my_ui <- fluidPage(
  
  useShinyjs(),
  
  uiOutput("app")
  
)

my_server <- function(input, output) {
  
  USER <- reactiveValues(Logged = FALSE)
  credentials <- list("test" = "123", 'ceu'='ceudata')  

  observeEvent(input$login, {
    if (isTRUE(credentials[[input$username]]==input$password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = "login",
                                textInput("username", "Username:"),
                                passwordInput("password", "Password:"),
                                div(actionButton("login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
    } else {
      # Sidebar with a slider input for number of bins
      wellPanel(
      uiOutput('my_ticker'),
      dateRangeInput('my_date',label = 'Date', start = '2018-01-01', end = Sys.Date()),
      dataTableOutput('my_data'),
      div(plotlyOutput('data_plot', width = '60%', height='800px'),align="center")
      )
      
    }
    
  )
  
  
  sp500 <-get_sp500()

  output$my_ticker <- renderUI({
    selectInput('ticker', label = 'select a ticker', choices = setNames(sp500$name, sp500$description), multiple = FALSE)
  })


  my_reactive_df <- reactive({
    df<- get_data_by_ticker_and_date(input$ticker, input$my_date[1], input$my_date[2])
    return(df)
  })


  # # go to https://rstudio.github.io/DT/shiny.html
  output$my_data <- DT::renderDataTable({
    my_render_df(my_reactive_df())
  })


  output$data_plot <- renderPlotly({
    get_plot_of_data(my_reactive_df())
  })


}

shinyApp(ui = my_ui, server = my_server)
