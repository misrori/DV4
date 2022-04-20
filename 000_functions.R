# These are the packages you will need during the first part of the class.

# install.packages("shiny")
# install.packages("jsonlite")
# install.packages("data.table")
# install.packages("httr")
# install.packages("rtsdata")
# install.packages("DT")
# install.packages("TTR")
# install.packages("plotly")
# install.packages("shinyjs")
# install.packages("ggplot2")
# install.packages("devtools")
# install.packages("rsconnect")
# install.packages("networkD3")


get_bmi_by_index_number <- function(bmi_index) {
  return(as.character(cut(
    bmi_index,
    breaks = c(0,18.5,25,30,35,40, Inf),
    labels = c('Below normal weight', 'Normal weight', 'Overweight', 'Class I Obesity', 'Class II Obesity', 'Class III Obesity'),
    right  = FALSE
  )))
}



get_tradingview_data <- function(json_string) {
  headers = c(
    `authority` = 'scanner.tradingview.com',
    `accept` = 'text/plain, */*; q=0.01',
    `origin` = 'https://www.tradingview.com',
    `user-agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36',
    `content-type` = 'application/x-www-form-urlencoded; charset=UTF-8',
    `sec-fetch-site` = 'same-site',
    `sec-fetch-mode` = 'cors',
    `referer` = 'https://www.tradingview.com/',
    `accept-encoding` = 'gzip, deflate, br',
    `accept-language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7'
  )
  
  res <- httr::POST(url = 'https://scanner.tradingview.com/america/scan', httr::add_headers(.headers=headers), body = json_string)
  
  t <- fromJSON(content(res, 'text'))
  df_data <-
    rbindlist(lapply(t$data$d, function(x){
      data.frame(t(data.frame(x)), stringsAsFactors = F)
    }))
  
  names(df_data) <-  fromJSON(json_string)$columns
  final_data <- cbind( data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  df_data)
  return(final_data)
  
}


get_sp500 <- function() {
  adat <- get_tradingview_data('{"filter":[{"left":"name","operation":"nempty"}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[],"groups":[{"type":"index","values":["SP:SPX"]}]},"columns":["name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","sector","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"name","sortOrder":"asc"},"range":[0,700]}')
  return(adat)
}


get_data_by_ticker_and_date  <- function(ticker, start_date, end_date) {
  
  tryCatch({
    my_data <- data.frame(ds.getSymbol.yahoo(ticker, from = (as.Date(start_date) -365), to =end_date ))
    names(my_data) <- tolower(sapply(strsplit(names(my_data), '.', fixed = T), '[[', 2))
    my_data$date <- as.Date(row.names(my_data)) 
    row.names(my_data) <- 1:nrow(my_data)
    my_data <- data.table(my_data)
    
    if( !identical(names(my_data) , c("open","high","low", "close","volume",  "adjusted","date"))) {
      text<- paste0('Error: ', my_ticker, ' # problem: names of dataframe is bad ', ' time: ', Sys.time())
      stop(text)
    }
    my_data$rsi <- round(RSI(my_data[['close']]),2)
    my_data[['sma_200_value']] <- SMA(my_data[['close']],200)
    my_data[['sma_50_value']] <- SMA(my_data[['close']], 50)
    my_data <- my_data[date>=start_date,]
    
    if ( nrow(my_data[complete.cases(my_data)==F,])> 0)  {
      my_data <- my_data[complete.cases(my_data),]
      if(nrow(my_data)==0){
        text<- paste0('Error: ', my_ticker, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
    return(my_data)
  }, error=function(x) {
    print(x)
    return(data.table())
  })
  
}


my_render_df <- function(my_data) {
  # https://rstudio.github.io/DT/extensions.html
  return(DT::datatable(my_data,extensions = c('Buttons','FixedHeader'),filter = 'top', class = 'cell-border stripe',
                       options = list(dom = 'Blfrtip',scrollX = TRUE, fixedHeader = TRUE,
                                      pageLength = 10,lengthMenu = c(10,50, 100, 500, 50000 ),
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))))
  
}






get_plot_of_data  <- function(data) {
  tryCatch({
    data$fall_from_max <- ((data$low / max(data$high)) -1 )*100
    data$fall_from_close <- ((data$close / max(data$high)) -1 )*100
    data$fall_from_high <- ((data$high / max(data$high)) -1 )*100
    
    
    
    fig <-plot_ly(data, x=~date, type="candlestick",
                  open = ~open, close = ~close,
                  high = ~high, low = ~low) %>% 
      layout(title = "Basic Candlestick Chart", showlegend = FALSE,  xaxis = list(rangeslider = list(visible = F))) %>% 
      add_lines(x=~date,y=~sma_200_value, name='200 daily MA') %>% 
      add_lines(x=~date,y=~sma_50_value, name='50 daily MA') 
    
    #fig
    
    fall_plot <- 
      plot_ly(data, x=~date,y=~fall_from_max,  type = 'scatter', mode = 'lines', name = 'min')  %>% 
      add_lines(x=~date,y=~fall_from_close, name='close') %>% 
      add_lines(x=~date,y=~fall_from_high, name='high')  %>% 
      layout(title = "", showlegend = F,  xaxis =list(title=""), yaxis =list(title="Fall from max (%)") )
    
    
    rsi_plot <- 
      plot_ly(data, x=~date,y=~rsi,  type = 'scatter', mode = 'lines', name = 'min') %>% 
      add_trace(x=~date,y=30, line = list(color = 'red', width = 4, dash = 'dash') ) %>% 
      add_trace(x=~date,y=70, line = list(color = 'red', width = 4, dash = 'dash')) %>% 
      layout(title = "", showlegend = F,  xaxis =list(title=""), yaxis =list(title="RSI") )
    
    all_p<- subplot(list(fig, rsi_plot, fall_plot ), nrows = 3, shareX = T, shareY = F, margin = 0.01, heights = c(0.5, 0.25, 0.25))
    return(all_p)
    
    
  },error=function(x){
    return(plotly_empty())
  })

}


get_ggplot_plot <- function(df) {
  ggplot(df, aes(date, close))+geom_line()+theme_bw()+labs(x='Date', y='Close price')
  
}

# download all_row
#
# output$all_tr <- DT::renderDT(server = FALSE, {
#   DT::datatable(
#    #your df,
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

