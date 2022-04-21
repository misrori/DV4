
library(shiny)
library(jsonlite)
library(data.table)
library(httr)
library(rtsdata)
library(DT)
library(TTR)
library(plotly)

source('000_functions.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("stock browser"),
  # selectInput('tickers', label = 'select a stock', choices = c('TSLA', 'AAPL')),
  dateRangeInput("dates", label = h3("Date range"),start = '2020-01-01', end = Sys.Date() ),
  uiOutput('tickerselector'),
  plotOutput('ggstoclplot'),
  plotlyOutput('plotly_stock'),
  DT::dataTableOutput('dt_render'),
  tableOutput('prices_df')
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  sp500 <- get_sp500()
  output$tickerselector <- renderUI({
    selectInput('ticker', label = 'select a stock', choices = setNames(  sp500$name  ,sp500$description)   )
  })
  
  df <- reactive({
    get_data_by_ticker_and_date(input$ticker, input$dates[1], input$dates[2] )
  })
  
  # output$prices_df <- renderTable(df() )
  output$dt_render <- DT::renderDT( {
    datatable(
      df(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  }  )

  output$ggstoclplot <- renderPlot({get_ggplot_plot(df() )})
  output$plotly_stock <- renderPlotly({ get_plot_of_data(df() )})
  # 
}

# Run the application 
shinyApp(ui = ui, server = server)

