library(shiny)
library(jsonlite)
library(data.table)
library(httr)
library(rtsdata)
library(DT)
library(TTR)
library(plotly)
source('000_functions.R')

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      dateRangeInput("dates", label = h3("Date range"),start = '2020-01-01', end = Sys.Date() ),
      
      uiOutput('select_dropdown')
      
    ), 
    mainPanel(
      plotOutput('ggstoclplot'),
      plotlyOutput('plotly_stock'),
      #tableOutput('stock_df')
      DT::dataTableOutput('dt_render')
    )
    
  )
  
  
  

)

server <- function(input, output, session) {

  sp_500 <- get_sp500()
  
  df <- reactive({
    get_data_by_ticker_and_date(input$ticker, input$dates[1], input$dates[2] )
  })
  
  output$ggstoclplot <- renderPlot({
    tryCatch({
      get_ggplot_plot(df() )
    },error=function(e){
      return(NULL)
      })
    })
  output$plotly_stock <- renderPlotly({ get_plot_of_data(df() )})

  output$dt_render <- DT::renderDT( {
    datatable(
      df(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  }  )
  
  output$select_dropdown <- renderUI({
    selectInput(inputId = 'ticker',label = 'select stock', choices = 
                  setNames(sp_500$name, sp_500$description )  )
  })
  
  # 
  # output$all_tr <- DT::renderDT(server = FALSE, {
  #   DT::datatable(
  #     df(),
  #     extensions = c("Buttons"), filter = "top",
  #     options = list(scrollX = TRUE,scrollY = TRUE,
  #                    dom = 'Bfrtip',
  #                    buttons = list(
  #                      list(extend = "excel", text = "Download", filename =  paste0('stock-data-', Sys.Date()),
  #                           exportOptions = list(
  #                             modifier = list(page = "all")
  #                           )
  #                      )
  #                    )
  #     )
  #   )
  # })
  # 
  
}

# setNames(sp500$name, sp500$description) 

shinyApp(ui, server)